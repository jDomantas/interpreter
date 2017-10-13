use std::collections::{BTreeSet, BTreeMap};
use std::collections::btree_map::Entry;
use std::fmt;
use codemap::Span;
use ast::{Node, Sym, Symbol};
use ast::resolved::{TypeDecl, Type, Items, Trait, TypeAnnot, Scheme, Impl};
use compiler::util::{self, Graph};
use CompileCtx;


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
    Value(&'a Node<Type>),
    Function(&'a Node<Type>, &'a Node<Type>),
    ShouldNotFail,
}

struct Constraint<'a>(Kind, Kind, ConstraintSource<'a>);

struct InferCtx<'a, 'b> {
    kinds: BTreeMap<Sym, Kind>,
    trait_kinds: BTreeMap<Sym, Kind>,
    next_var: u64,
    ctx: &'b mut CompileCtx,
    constraints: Vec<Constraint<'a>>,
    new_kinds: BTreeMap<Sym, Kind>,
    var_kinds: BTreeMap<Sym, Kind>,
    substitutions: BTreeMap<u64, Kind>,
}

impl<'a, 'b> InferCtx<'a, 'b> {
    fn new(ctx: &'b mut CompileCtx) -> Self {
        InferCtx {
            kinds: BTreeMap::new(),
            trait_kinds: BTreeMap::new(),
            // start from 1 because 0 is used as id for `self`
            next_var: 1,
            ctx,
            constraints: Vec::new(),
            new_kinds: BTreeMap::new(),
            var_kinds: BTreeMap::new(),
            substitutions: BTreeMap::new(),
        }
    }

    fn unsolved_constraint(&mut self, constraint: Constraint) {
        let mut kinds = [constraint.0, constraint.1];
        rename_vars(&mut kinds);
        let (message, span) = match constraint.2 {
            ConstraintSource::ShouldNotFail => {
                panic!("failed to solve kind constraint \
                        that should not have failed")
            }
            ConstraintSource::Value(type_) => {
                debug_assert_eq!(kinds[1], Kind::Star);
                let msg = format!(
                    "Previously inferred kind `{}` for this type, but it must have kind `*`.",
                    kinds[0]);
                (msg, type_.span)
            }
            ConstraintSource::Function(f, _) => {
                let msg = format!(
                    "Previously inferred kind `{}` for this type, but here it is expected to be `{}`.",
                    kinds[0],
                    kinds[1]);
                (msg, f.span)
            }
        };
        self.ctx.reporter
            .kind_error(message.as_str(), span)
            .span_note(message, span)
            .done();
    }

    fn invalid_self(&mut self, span: Span) {
        let msg = "`self` type can only be used in trait definitions.";
        self.ctx.reporter
            .kind_error(msg, span)
            .span_note(msg, span)
            .done();
    }

    fn missing_self(&mut self, span: Span) {
        let msg = "`self` must be mentioned at least once in every trait member.";
        self.ctx.reporter
            .kind_error(msg, span)
            .span_note(msg, span)
            .done();
    }

    fn unused_var(&mut self, span: Span) {
        let msg = "Constrained type variable must be used in implementing type.";
        self.ctx.reporter
            .kind_error(msg, span)
            .span_note(msg, span)
            .done();
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
            let mut current_kind = self.new_kinds[&decl.name()].clone();
            for var in decl.var_list() {
                let kind = self.fresh_var();
                let v = self.fresh_var();
                let arr = Kind::arr(&kind, &v);
                let source = ConstraintSource::ShouldNotFail;
                self.constraints.push(Constraint(current_kind, arr, source));
                current_kind = v;
                if self.var_kinds.contains_key(&var.value) {
                    self.var_kinds.insert(var.value, Kind::Any);
                } else {
                    self.var_kinds.insert(var.value, kind);
                }
            }
            let source = ConstraintSource::ShouldNotFail;
            self.constraints.push(Constraint(current_kind, Kind::Star, source));
            for type_ in contained_types(decl) {
                let kind = self.infer_for_type(type_, false);
                let source = ConstraintSource::Value(&type_);
                self.constraints.push(Constraint(kind, Kind::Star, source));
            }
            self.var_kinds.clear();
        }

        let ok = self.solve_constraints();
        let mut new_kinds = BTreeMap::new();
        ::std::mem::swap(&mut self.new_kinds, &mut new_kinds);
        for (name, kind) in new_kinds {
            let mut kind = self.do_substitutions(kind);
            kind.default_vars();
            let kind = if ok { kind } else { Kind::Any };
            self.kinds.insert(name, kind);
        }
        self.constraints.clear();
        self.substitutions.clear();
    }

    fn infer_for_type(
        &mut self,
        type_: &'a Node<Type>,
        allow_new_vars: bool,
    ) -> Kind {
        match type_.value {
            Type::Any => {
                Kind::Any
            }
            Type::Var(sym) => {
                if self.var_kinds.contains_key(&sym) {
                    self.var_kinds[&sym].clone()
                } else if allow_new_vars {
                    let kind = self.fresh_var();
                    self.var_kinds.insert(sym, kind.clone());
                    kind
                } else {
                    unreachable!()
                }
            }
            Type::Tuple(ref items) => {
                for item in items {
                    let kind = self.infer_for_type(item, allow_new_vars);
                    let source = ConstraintSource::Value(&item);
                    self.constraints.push(Constraint(kind, Kind::Star, source));
                }
                Kind::Star
            }
            Type::SelfType => {
                let self_ = Sym(0);
                if self.var_kinds.contains_key(&self_) {
                    self.var_kinds[&self_].clone()
                } else {
                    self.invalid_self(type_.span);
                    Kind::Any
                }
            }
            Type::Function(ref a, ref b) => {
                let a_kind = self.infer_for_type(a, allow_new_vars);
                let b_kind = self.infer_for_type(b, allow_new_vars);
                let source = ConstraintSource::Value(&a);
                self.constraints.push(Constraint(a_kind, Kind::Star, source));
                let source = ConstraintSource::Value(&b);
                self.constraints.push(Constraint(b_kind, Kind::Star, source));
                Kind::Star
            }
            Type::Concrete(Symbol::Unknown) => {
                Kind::Any
            }
            Type::Concrete(Symbol::Known(sym)) => {
                if self.kinds.contains_key(&sym) {
                    self.kinds[&sym].clone()
                } else {
                    self.new_kinds[&sym].clone()
                }
            }
            Type::Apply(ref a, ref b) => {
                let a_kind = self.infer_for_type(a, allow_new_vars);
                let b_kind = self.infer_for_type(b, allow_new_vars);
                let var = self.fresh_var();
                let arr = Kind::arr(&b_kind, &var);
                let source = ConstraintSource::Function(&a, &b);
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

    fn solve_constraint<'c>(&mut self, a: &'c Kind, b: &'c Kind) -> bool {
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
        self.var_kinds.insert(Sym(0), self_kind);
        for annot in &trait_.values {
            let scheme = &annot.value.type_;
            self.infer_for_type(&scheme.value.type_, true);
            if !scheme.value.type_.value.contains_self() {
                self.missing_self(annot.span);
            }
        }
        let self_kind = if self.solve_constraints() {
            let kind = self.var_kinds.remove(&Sym(0)).unwrap();
            let mut kind = self.do_substitutions(kind);
            kind.default_vars();
            kind
        } else {
            Kind::Any
        };
        self.constraints.clear();
        self.var_kinds.clear();
        self.substitutions.clear();
        // println!("inferred trait {} : {}", trait_.name.value, self_kind);
        self_kind
    }

    fn infer_traits(&mut self, traits: &'a [Trait]) {
        for trait_ in traits {
            let kind = self.infer_trait_kind(trait_);
            self.trait_kinds.insert(trait_.name.value, kind);
        }
    }

    fn add_trait_bounds(&mut self, bounds: &'a [(Node<Sym>, Node<Symbol>)]) {
        for &(ref var, ref trait_) in bounds {
            let trait_name = match trait_.value {
                Symbol::Known(sym) => sym,
                Symbol::Unknown => continue,
            };
            let kind = match self.trait_kinds.get(&trait_name) {
                Some(kind) => kind,
                None => continue,
            };
            match self.var_kinds.entry(var.value) {
                Entry::Occupied(mut entry) => {
                    if entry.get() != kind {
                        if entry.get() != &Kind::Any {
                            let message = format!(
                                "Variable {} here has kind `{}`, but in previous bound it was bound to `{}`.",
                                self.ctx.symbols.symbol_name(var.value),
                                kind,
                                entry.get());
                            self.ctx.reporter
                                .kind_error(message.as_str(), var.span)
                                .span_note(message, var.span)
                                .done();
                        }
                        entry.insert(Kind::Any);
                    }
                }
                Entry::Vacant(entry) => {
                    entry.insert(kind.clone());
                }
            }
        }
    }

    fn validate_scheme(&mut self, scheme: &'a Scheme) {
        self.add_trait_bounds(&scheme.bounds);
        let kind = self.infer_for_type(&scheme.type_, true);
        let source = ConstraintSource::Value(&scheme.type_);
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
        self.validate_scheme(&annot.type_.value);
    }

    fn validate_trait(&mut self, trait_: &'a Trait) {
        debug_assert!(self.constraints.is_empty());
        debug_assert!(self.new_kinds.is_empty());
        debug_assert!(self.var_kinds.is_empty());
        debug_assert!(self.substitutions.is_empty());
        let self_kind = self.trait_kinds[&trait_.name.value].clone();
        for base_trait in &trait_.base_traits {
            let trait_name = match base_trait.value {
                Symbol::Known(sym) => sym,
                Symbol::Unknown => continue,
            };
            if let Some(kind) = self.trait_kinds.get(&trait_name) {
                if self_kind != *kind && self_kind != Kind::Any && *kind != Kind::Any {
                    let message = format!(
                        "Trait `{}` has kind `{}`, but its parent trait has kind `{}`.",
                        self.ctx.symbols.symbol_name(trait_.name.value),
                        self_kind,
                        kind);
                    self.ctx.reporter
                        .kind_error(message.as_str(), base_trait.span)
                        .span_note(message, base_trait.span)
                        .done();
                }
            }
        }
        for annot in &trait_.values {
            self.var_kinds.insert(Sym(0), self_kind.clone());
            self.validate_scheme(&annot.value.type_.value);
        }
    }

    fn validate_impl(&mut self, impl_: &'a Impl) {
        debug_assert!(self.constraints.is_empty());
        debug_assert!(self.new_kinds.is_empty());
        debug_assert!(self.var_kinds.is_empty());
        debug_assert!(self.substitutions.is_empty());
        self.add_trait_bounds(&impl_.scheme.value.bounds);
        let mut checked_vars = BTreeSet::new();
        for &(ref var, _) in &impl_.scheme.value.bounds {
            if !checked_vars.contains(&var.value) {
                checked_vars.insert(&var.value);
                if !impl_.scheme.value.type_.value.contains_var(var.value) {
                    self.unused_var(var.span);
                }
            }
        }
        let kind = self.infer_for_type(&impl_.scheme.value.type_, true);
        if self.solve_constraints() {
            let mut kind = self.do_substitutions(kind);
            kind.default_vars();
            if let Symbol::Known(trait_name) = impl_.trait_.value {
                if let Some(trait_) = self.trait_kinds.get(&trait_name) {
                    if trait_ != &Kind::Any && trait_ != &kind {
                        let message = format!(
                            "Inferred kind `{}` for this type, but trait expected it to be `{}`.",
                            kind,
                            trait_);
                        self.ctx.reporter
                            .kind_error(message.as_str(), impl_.scheme.value.type_.span)
                            .span_note(message, impl_.scheme.value.type_.span)
                            .done();
                    }
                }
            }
        }
        self.var_kinds.clear();
        self.substitutions.clear();
    }
}

fn rename_vars(kinds: &mut [Kind]) {
    fn add_vars(kind: &Kind, result: &mut BTreeMap<u64, u64>, next: &mut u64) {
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
    let mut mapping = BTreeMap::new();
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
        TypeDecl::TypeAlias(ref alias) => {
            alias.type_.iter().collect()
        }
        TypeDecl::Union(ref union) => {
            union.cases.iter().flat_map(|case| case.value.args.iter()).collect()
        }
    }
}

fn contained_concrete_types<'a>(decl: &'a TypeDecl) -> Vec<&'a Sym> {
    let mut result = Vec::new();
    match *decl {
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

fn make_graph<'a, I: Iterator<Item=&'a TypeDecl>>(decls: I) -> Graph<'a, Sym> {
    let nodes = decls.map(|decl| {
        let depends_on = contained_concrete_types(decl);
        let name = match *decl {
            TypeDecl::TypeAlias(ref alias) => &alias.name.value,
            TypeDecl::Union(ref union) => &union.name.value,
        };
        (name, depends_on)
    });
    Graph::new(nodes)
}

pub(crate) fn find_kind_errors(items: &Items, ctx: &mut CompileCtx) {
    let graph = make_graph(items.types.iter());
    let components = graph.to_strongly_connected_components();
    let table = items.types.iter().map(|decl| (decl.name(), decl)).collect::<BTreeMap<_, _>>();
    
    let mut inferer = InferCtx::new(ctx);
    for scc in components {
        let decls = scc.into_iter().map(|name| table[name]).collect::<Vec<_>>();
        inferer.add_types(&decls);
    }
    inferer.infer_traits(&items.traits);
    for annot in items.annotations.values() {
        inferer.validate_annotation(&annot.value);
    }
    for trait_ in &items.traits {
        inferer.validate_trait(trait_);
    }
    for impl_ in &items.impls {
        inferer.validate_impl(impl_);
    }
}
