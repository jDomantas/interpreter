use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::fmt;
use ast::Node;
use ast::resolved::{TypeDecl, Type, Items, Trait, TypeAnnot, Scheme};
use compiler::util::{self, Graph};
use errors::{self, Error};
use position::Span;


#[derive(PartialEq, Eq, Debug, Clone)]
enum Kind {
    Var(u64),
    Star,
    Any,
    Arrow(Box<Kind>, Box<Kind>),
}

impl Kind {
    fn is_arrow(&self) -> bool {
        match *self {
            Kind::Arrow(_, _) => true,
            _ => false,
        }
    }

    fn arr(a: &Kind, b: &Kind) -> Kind {
        Kind::Arrow(Box::new(a.clone()), Box::new(b.clone()))
    }

    fn contains_var(&self, var: u64) -> bool {
        match *self {
            Kind::Var(v) => var == v,
            Kind::Star | Kind::Any => false,
            Kind::Arrow(ref a, ref b) => a.contains_var(var) || b.contains_var(var),
        }
    }

    fn substitute_vars<F: Fn(u64) -> Option<Kind>>(&mut self, f: &F) {
        match *self {
            Kind::Var(v) => *self = f(v).unwrap_or(Kind::Var(v)),
            Kind::Star | Kind::Any => { },
            Kind::Arrow(ref mut a, ref mut b) => {
                a.substitute_vars(f);
                b.substitute_vars(f);
            }
        }
    }

    fn default_vars(&mut self) {
        match *self {
            Kind::Var(_) => *self = Kind::Star,
            Kind::Arrow(ref mut a, ref mut b) => {
                a.default_vars();
                b.default_vars();
            }
            Kind::Star | Kind::Any => { }
        }
    }
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Kind::Var(index) => {
                write!(f, "k{}", index)
            }
            Kind::Star => {
                write!(f, "*")
            }
            Kind::Any => {
                write!(f, "?")
            }
            Kind::Arrow(ref left, ref right) => {
                if left.is_arrow() {
                    write!(f, "({}) -> {}", left, right)
                } else {
                    write!(f, "{} -> {}", left, right)
                }
            }
        }
    }
}

enum ConstraintSource<'a> {
    Value(&'a Node<Type>, &'a str),
    Function(&'a Node<Type>, &'a Node<Type>, &'a str),
    ShouldNotFail,
}

struct Constraint<'a>(Kind, Kind, ConstraintSource<'a>);

struct InferCtx<'a> {
    kinds: HashMap<&'a str, Kind>,
    trait_kinds: HashMap<&'a str, Kind>,
    next_var: u64,
    errors: Vec<Error>,
    constraints: Vec<Constraint<'a>>,
    new_kinds: HashMap<&'a str, Kind>,
    var_kinds: HashMap<&'a str, Kind>,
    substitutions: HashMap<u64, Kind>,
}

impl<'a> InferCtx<'a> {
    fn new() -> Self {
        InferCtx {
            kinds: HashMap::new(),
            trait_kinds: HashMap::new(),
            next_var: 0,
            errors: Vec::new(),
            constraints: Vec::new(),
            new_kinds: HashMap::new(),
            var_kinds: HashMap::new(),
            substitutions: HashMap::new(),
        }
    }

    fn undefined_var(&mut self, var: &Node<String>, type_: &str) {
        let message = format!("Unknown type variable '{}'.", var.value);
        let module = errors::symbol_module(type_);
        let error = errors::kind_error(message, var.span, module);
        self.errors.push(error);
    }

    fn var_defined_twice(&mut self, var: &Node<String>, type_: &str) {
        let message = format!("Type variable '{}' appears twice in var list.",
            var.value);
        let module = errors::symbol_module(type_);
        let error = errors::kind_error(message, var.span, module);
        self.errors.push(error);
    }

    fn unsolved_constraint(&mut self, constraint: Constraint) {
        let mut kinds = [constraint.0, constraint.1];
        rename_vars(&mut kinds);
        let (message, owner, span) = match constraint.2 {
            ConstraintSource::ShouldNotFail => {
                panic!("failed to solve kind constraint \
                        that should not have failed")
            }
            ConstraintSource::Value(type_, owner) => {
                debug_assert_eq!(kinds[1], Kind::Star);
                let msg = format!(
                    "Previously inferred kind '{}' for this type, but it must have kind '*'.",
                    kinds[0]);
                (msg, owner, type_.span)
            }
            ConstraintSource::Function(f, _, owner) => {
                let msg = format!(
                    "Previously inferred kind '{}' for this type, but here it is expected to be '{}'.",
                    kinds[0],
                    kinds[1]);
                (msg, owner, f.span)
            }
        };
        let module = errors::symbol_module(owner);
        let error = errors::kind_error(message, span, module);
        self.errors.push(error);
    }

    fn invalid_self(&mut self, span: Span, type_: &str) {
        let message = format!("'self' type can only be used in trait definitions.");
        let module = errors::symbol_module(type_);
        let error = errors::kind_error(message, span, module);
        self.errors.push(error);
    }

    fn missing_self(&mut self, span: Span, type_: &str) {
        let message = format!("'self' must be mentioned at least once in trait members.");
        let module = errors::symbol_module(type_);
        let error = errors::kind_error(message, span, module);
        self.errors.push(error);
    }

    fn fresh_var(&mut self) -> Kind {
        let kind = Kind::Var(self.next_var);
        self.next_var += 1;
        kind
    }

    fn add_types(&mut self, decls: &[&'a TypeDecl]) {
        debug_assert!(self.constraints.is_empty());
        debug_assert!(self.new_kinds.is_empty());
        debug_assert!(self.var_kinds.is_empty());
        debug_assert!(self.substitutions.is_empty());
        for decl in decls {
            let var = self.fresh_var();
            self.new_kinds.insert(decl.name(), var);
        }
        for decl in decls {
            let mut current_kind = self.new_kinds[decl.name()].clone();
            for var in decl.var_list() {
                let kind = self.fresh_var();
                let v = self.fresh_var();
                let arr = Kind::arr(&kind, &v);
                let source = ConstraintSource::ShouldNotFail;
                self.constraints.push(Constraint(current_kind, arr, source));
                current_kind = v;
                if self.var_kinds.contains_key(&var.value as &str) {
                    self.var_kinds.insert(&var.value, Kind::Any);
                    self.var_defined_twice(var, decl.name());
                } else {
                    self.var_kinds.insert(&var.value, kind);
                }
            }
            let source = ConstraintSource::ShouldNotFail;
            self.constraints.push(Constraint(current_kind, Kind::Star, source));
            for type_ in contained_types(decl) {
                let kind = self.infer_for_type(type_, decl.name(), false);
                let source = ConstraintSource::Value(&type_, decl.name());
                self.constraints.push(Constraint(kind, Kind::Star, source));
            }
            self.var_kinds.clear();
        }

        let ok = self.solve_constraints();
        let mut new_kinds = HashMap::new();
        ::std::mem::swap(&mut self.new_kinds, &mut new_kinds);
        for (name, kind) in new_kinds.drain() {
            let mut kind = self.do_substitutions(kind);
            kind.default_vars();
            let kind = if ok { kind } else { Kind::Any };
            println!("inferred {} : {}", name, kind);
            self.kinds.insert(name, kind);
        }
        self.constraints.clear();
        self.substitutions.clear();
    }

    fn infer_for_type(
                        &mut self,
                        type_: &'a Node<Type>,
                        owning_type: &'a str,
                        allow_new_vars: bool) -> Kind {
        match type_.value {
            Type::Var(ref var) => {
                if self.var_kinds.contains_key(var as &str) {
                    self.var_kinds[var as &str].clone()
                } else if allow_new_vars {
                    let kind = self.fresh_var();
                    self.var_kinds.insert(var, kind.clone());
                    kind
                } else {
                    self.undefined_var(&Node::new(var.clone(), type_.span), owning_type);
                    Kind::Any
                }
            }
            Type::Tuple(ref items) => {
                for item in items {
                    let kind = self.infer_for_type(item, owning_type, allow_new_vars);
                    let source = ConstraintSource::Value(&item, owning_type);
                    self.constraints.push(Constraint(kind, Kind::Star, source));
                }
                Kind::Star
            }
            Type::SelfType => {
                if self.var_kinds.contains_key("self") {
                    self.var_kinds["self"].clone()
                } else {
                    self.invalid_self(type_.span, owning_type);
                    Kind::Any
                }
            }
            Type::Function(ref a, ref b) => {
                let a_kind = self.infer_for_type(a, owning_type, allow_new_vars);
                let b_kind = self.infer_for_type(b, owning_type, allow_new_vars);
                let source = ConstraintSource::Value(&a, owning_type);
                self.constraints.push(Constraint(a_kind, Kind::Star, source));
                let source = ConstraintSource::Value(&b, owning_type);
                self.constraints.push(Constraint(b_kind, Kind::Star, source));
                Kind::Star
            }
            Type::Concrete(ref name) => {
                if self.kinds.contains_key(name as &str) {
                    self.kinds[name as &str].clone()
                } else {
                    self.new_kinds[name as &str].clone()
                }
            }
            Type::Apply(ref a, ref b) => {
                let a_kind = self.infer_for_type(a, owning_type, allow_new_vars);
                let b_kind = self.infer_for_type(b, owning_type, allow_new_vars);
                let var = self.fresh_var();
                let arr = Kind::arr(&b_kind, &var);
                let source = ConstraintSource::Function(&a, &b, owning_type);
                self.constraints.push(Constraint(a_kind, arr, source));
                var
            }
        }
    }

    fn solve_constraints(&mut self) -> bool {
        debug_assert!(self.substitutions.is_empty());
        let mut constraints = Vec::new();
        ::std::mem::swap(&mut constraints, &mut self.constraints);
        for constraint in constraints {
            let a = self.do_substitutions(constraint.0);
            let b = self.do_substitutions(constraint.1);
            if !self.solve_constraint(&a, &b) {
                self.unsolved_constraint(Constraint(a, b, constraint.2));
                return false;
            }
        }
        true
    }

    fn solve_constraint<'b>(&mut self, a: &'b Kind, b: &'b Kind) -> bool {
        match (a, b) {
            (&Kind::Any, _) | (_, &Kind::Any) => true,
            (&Kind::Var(a), &Kind::Var(b)) if a == b => true,
            (&Kind::Star, &Kind::Star) => true,
            (&Kind::Var(v), k) | (k, &Kind::Var(v)) => {
                if k.contains_var(v) {
                    false
                } else {
                    self.add_substitution(v, k.clone());
                    true
                }
            }
            (&Kind::Arrow(ref a1, ref a2), &Kind::Arrow(ref b1, ref b2)) => {
                self.solve_constraint(a1, b1) && {
                    let a2 = self.do_substitutions((**a2).clone());
                    let b2 = self.do_substitutions((**b2).clone());
                    self.solve_constraint(&a2, &b2)
                }
            }
            (_, _) => {
                false
            }
        }
    }

    fn add_substitution(&mut self, var: u64, kind: Kind) {
        let kind = self.do_substitutions(kind);
        for (_, k) in &mut self.substitutions {
            k.substitute_vars(&(|v| {
                if v == var {
                    Some(kind.clone())
                } else {
                    None
                }
            }));
        }
        self.substitutions.insert(var, kind);
    }

    fn do_substitutions(&self, kind: Kind) -> Kind {
        let mut kind = kind.clone();
        kind.substitute_vars(&(|var| self.substitutions.get(&var).cloned()));
        kind
    }

    fn infer_trait_kind(&mut self, trait_: &'a Trait) -> Kind {
        debug_assert!(self.constraints.is_empty());
        debug_assert!(self.new_kinds.is_empty());
        debug_assert!(self.var_kinds.is_empty());
        debug_assert!(self.substitutions.is_empty());
        let self_kind = self.fresh_var();
        self.var_kinds.insert("self", self_kind);
        for annot in &trait_.values {
            if let Some(ref type_) = annot.value.type_ {
                self.infer_for_type(&type_.value.type_, &trait_.name.value, true);
                if !type_.value.type_.value.contains_self() {
                    self.missing_self(type_.span, &trait_.name.value);
                }
            }
        }
        let self_kind = if self.solve_constraints() {
            let kind = self.var_kinds.remove("self").unwrap();
            let mut kind = self.do_substitutions(kind);
            kind.default_vars();
            kind
        } else {
            Kind::Any
        };
        self.constraints.clear();
        self.var_kinds.clear();
        self.substitutions.clear();
        println!("inferred trait {} : {}", trait_.name.value, self_kind);
        self_kind
    }

    fn infer_traits(&mut self, traits: &'a [Trait]) {
        for trait_ in traits {
            let kind = self.infer_trait_kind(trait_);
            self.trait_kinds.insert(&trait_.name.value, kind);
        }
    }

    fn validate_scheme(&mut self, scheme: &'a Scheme, owner: &'a str) {
        for &(ref var, ref trait_) in &scheme.bounds {
            let kind = match self.trait_kinds.get(&trait_.value as &str) {
                Some(kind) => kind,
                None => continue,
            };
            match self.var_kinds.entry(&var.value) {
                Entry::Occupied(mut entry) => {
                    if entry.get() != kind {
                        if entry.get() != &Kind::Any {
                            // Error function was inlined here to appease borrow checker.
                            // I think it would be a good idea to make a sepparate struct
                            // for error formatting, and have that as field, so that
                            // reporting an error would only borrow one field, instead
                            // of whole self.
                            let message = format!(
                                "Variable {} here has kind '{}', but in previous bound it was bound to '{}'.",
                                var.value,
                                kind,
                                entry.get());
                            let module = errors::symbol_module(owner);
                            let error = errors::kind_error(message, var.span, module);
                            self.errors.push(error);
                        }
                        entry.insert(Kind::Any);
                    }
                }
                Entry::Vacant(entry) => {
                    entry.insert(kind.clone());
                }
            }
        }
        let kind = self.infer_for_type(&scheme.type_, owner, true);
        let source = ConstraintSource::Value(&scheme.type_, owner);
        self.constraints.push(Constraint(kind, Kind::Star, source));
        self.solve_constraints();
        self.var_kinds.clear();
        self.substitutions.clear();
    }

    fn validate_annotation(&mut self, annot: &'a TypeAnnot) {
        debug_assert!(self.constraints.is_empty());
        debug_assert!(self.new_kinds.is_empty());
        debug_assert!(self.var_kinds.is_empty());
        debug_assert!(self.substitutions.is_empty());
        if let Some(ref type_) = annot.type_ {
            self.validate_scheme(&type_.value, &annot.value.value);
        }
    }

    fn validate_trait(&mut self, trait_: &'a Trait) {
        debug_assert!(self.constraints.is_empty());
        debug_assert!(self.new_kinds.is_empty());
        debug_assert!(self.var_kinds.is_empty());
        debug_assert!(self.substitutions.is_empty());
        let self_kind = self.trait_kinds[&trait_.name.value as &str].clone();
        for base_trait in &trait_.base_traits {
            if let Some(kind) = self.trait_kinds.get(&base_trait.value as &str) {
                if self_kind != *kind && self_kind != Kind::Any && *kind != Kind::Any {
                    // Error function was inlined here to appease borrow checker.
                    // I think it would be a good idea to make a sepparate struct
                    // for error formatting, and have that as field, so that
                    // reporting an error would only borrow one field, instead
                    // of whole self.
                    let message = format!(
                        "Trait '{}' has kind '{}', but its parent trait has kind '{}'.",
                        trait_.name.value,
                        self_kind,
                        kind);
                    let module = errors::symbol_module(trait_.name.value.clone());
                    let error = errors::kind_error(message, base_trait.span, module);
                    self.errors.push(error);
                }
            }
        }
        for annot in &trait_.values {
            if let Some(ref type_) = annot.value.type_ {
                self.var_kinds.insert("self", self_kind.clone());
                self.validate_scheme(&type_.value, &trait_.name.value);
            }
        }
    }
}

fn rename_vars(kinds: &mut [Kind]) {
    fn add_vars(kind: &Kind, result: &mut HashMap<u64, u64>, next: &mut u64) {
        match *kind {
            Kind::Any | Kind::Star => { }
            Kind::Var(var) => {
                if let Entry::Vacant(e) = result.entry(var) {
                    e.insert(*next);
                    *next += 1;
                }
            }
            Kind::Arrow(ref a, ref b) => {
                add_vars(a, result, next);
                add_vars(b, result, next);
            }
        }
    }
    let mut mapping = HashMap::new();
    let mut next = 1;
    for kind in kinds.iter() {
        add_vars(kind, &mut mapping, &mut next);
    }
    for kind in kinds.iter_mut() {
        kind.substitute_vars(&(|var| Some(Kind::Var(mapping[&var]))));
    }
}

fn contained_types(decl: &TypeDecl) -> Vec<&Node<Type>> {
    match *decl {
        TypeDecl::Record(ref record) => {
            record.fields.iter().map(|&(_, ref t)| t).collect()
        }
        TypeDecl::TypeAlias(ref alias) => {
            alias.type_.iter().collect()
        }
        TypeDecl::Union(ref union) => {
            union.cases.iter().flat_map(|case| case.value.args.iter()).collect()
        }
    }
}

fn contained_concrete_types(decl: &TypeDecl) -> Vec<&str> {
    let mut result = Vec::new();
    match *decl {
        TypeDecl::Record(ref record) => {
            for &(_, ref type_) in &record.fields {
                util::collect_concrete_types(type_, &mut result);
            }
        }
        TypeDecl::TypeAlias(ref alias) => {
            if let Some(ref type_) = alias.type_ {
                util::collect_concrete_types(type_, &mut result);
            }
        }
        TypeDecl::Union(ref union) => {
            for case in &union.cases {
                for type_ in &case.value.args {
                    util::collect_concrete_types(type_, &mut result);
                }
            }
        }
    }
    result
}

fn make_graph<'a, I: Iterator<Item=&'a TypeDecl>>(decls: I) -> Graph<'a, str> {
    let nodes = decls.map(|decl| {
        let depends_on = contained_concrete_types(decl);
        let name = match *decl {
            TypeDecl::Record(ref record) => &record.name.value,
            TypeDecl::TypeAlias(ref alias) => &alias.name.value,
            TypeDecl::Union(ref union) => &union.name.value,
        };
        (name.as_ref(), depends_on)
    });
    Graph::new(nodes)
}

pub fn find_kind_errors(items: &Items) -> Vec<Error> {
    let graph = make_graph(items.types.iter());
    let components = graph.to_strongly_connected_components();
    let table = items.types.iter().map(|decl| (decl.name(), decl)).collect::<HashMap<_, _>>();
    let mut inferer = InferCtx::new();
    for scc in components {
        let decls = scc.into_iter().map(|name| table[name]).collect::<Vec<_>>();
        inferer.add_types(&decls);
    }
    inferer.infer_traits(&items.traits);
    for annot in items.annotations.values() {
        inferer.validate_annotation(annot);
    }
    for trait_ in &items.traits {
        inferer.validate_trait(trait_);
    }
    inferer.errors
}
