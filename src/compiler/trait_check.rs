use std::collections::{HashMap, HashSet};
use ast::{Name, Node};
use ast::typed::{
    Expr, Impl, Impls, Sym, Symbol, Items, Scheme, ImplSource, Type, Def,
};
use compiler::builtins;
use errors::Errors;
use position::Span;


/// Make mapping from variables in principal type to types in concrete type.
fn get_instantiation<'a>(principal: &Type, concrete: &'a Type) -> Option<HashMap<u64, &'a Type>> {
    fn walk<'a>(a: &Type, b: &'a Type, mapping: &mut HashMap<u64, &'a Type>) -> bool {
        match (a, b) {
            (&Type::Any, _) => true,
            (_, &Type::Any) => true,
            (&Type::Var(var), t) => {
                *mapping.entry(var).or_insert(t) == t
            }
            (&Type::Apply(ref a1, ref a2), &Type::Apply(ref b1, ref b2)) |
            (&Type::Function(ref a1, ref a2), &Type::Function(ref b1, ref b2)) => {
                walk(a1, b1, mapping) && walk(a2, b2, mapping)
            }
            (&Type::Tuple(ref a), &Type::Tuple(ref b)) => {
                a.len() == b.len() && {
                    for (a, b) in a.iter().zip(b.iter()) {
                        if !walk(a, b, mapping) {
                            return false;
                        }
                    }
                    true
                }
            }
            (&Type::Concrete(a), &Type::Concrete(b)) => {
                a == b
            }
            _ => false,
        }
    }
    let mut mapping = HashMap::new();
    if walk(principal, concrete, &mut mapping) {
        Some(mapping)
    } else {
        None
    }
}

struct Bound<'a> {
    var: u64,
    instantiated_to: &'a Type,
    trait_: Sym,
}

struct Location {
    span: Span,
    module: Name,
}

/// Get trait bounds that types in concrete type have to statisfy to allow
/// instantiating principal type as concrete.
fn get_trait_bounds<'a>(principal: &Scheme, concrete: &'a Type) -> Option<Vec<Bound<'a>>> {
    let instantiation = match get_instantiation(&principal.type_, concrete) {
        Some(instantiation) => instantiation,
        None => return None,
    };
    let mut bounds = Vec::new();
    for var in &principal.vars {
        let instantiated_to = *instantiation.get(&var.id).unwrap();
        for &bound in &var.bounds {
            bounds.push(Bound {
                var: var.id,
                instantiated_to,
                trait_: bound,
            });
        }
    }
    Some(bounds)
}

struct SolverCtx<'a, 'b> {
    impls: &'a [Impl],
    errors: &'b mut Errors,
    known_impls: &'a HashSet<(u64, Sym)>,
    symbol_names: &'a HashMap<Sym, String>,
    symbol_types: &'a HashMap<Sym, Scheme>,
    module: Name,
}

impl<'a, 'b> SolverCtx<'a, 'b> {
    fn new(
            impls: &'a [Impl],
            errors: &'b mut Errors,
            known_impls: &'a HashSet<(u64, Sym)>,
            symbol_names: &'a HashMap<Sym, String>,
            symbol_types: &'a HashMap<Sym, Scheme>,
            module: Name) -> Self {
        SolverCtx {
            impls,
            errors,
            known_impls,
            symbol_names,
            symbol_types,
            module,
        }
    }

    fn solve_constraint(&mut self, type_: &Type, trait_: Sym, span: Span) -> Option<ImplSource> {
        match *type_ {
            Type::Any => None,
            Type::Var(var) => {
                if self.known_impls.contains(&(var, trait_)) {
                    Some(ImplSource::FromContext(var, trait_))
                } else {
                    // TODO: somehow give proper names to type variables
                    let msg = format!(
                        "Type `t{}` does not implement trait `{}`.",
                        var,
                        self.symbol_names[&trait_]);
                    self.errors
                        .trait_error(&self.module)
                        .note(msg, span)
                        .done();
                    None
                }
            }
            Type::Tuple(ref items) if
                trait_ == builtins::traits::EQ ||
                trait_ == builtins::traits::ORD ||
                trait_ == builtins::traits::TO_STRING => {
                let mut subimpls = Vec::new();
                let mut all_ok = true;
                for item in items {
                    if let Some(impl_) = self.solve_constraint(item, trait_, span) {
                        subimpls.push(impl_);
                    } else {
                        all_ok = false;
                    }
                }
                if all_ok {
                    match trait_ {
                        builtins::traits::EQ =>
                            Some(ImplSource::TupleEq(subimpls)),
                        builtins::traits::ORD => 
                            Some(ImplSource::TupleOrd(subimpls)),
                        builtins::traits::TO_STRING => 
                            Some(ImplSource::TupleToString(subimpls)),
                        _ => unreachable!(),
                    }
                } else {
                    None
                }
            }
            ref t => {
                for impl_ in self.impls {
                    if impl_.trait_.value != Symbol::Known(trait_) {
                        continue;
                    }
                    if let Some(bounds) = get_trait_bounds(&impl_.scheme.value, t) {
                        let mut impls = HashMap::new();
                        let mut all_ok = true;
                        for bound in bounds {
                            if let Some(source) = self.solve_constraint(
                                bound.instantiated_to,
                                bound.trait_,
                                span)
                            {
                                impls.insert((bound.var, bound.trait_), source);
                            } else {
                                all_ok = false;
                            }
                        }
                        if all_ok {
                            return Some(ImplSource::Apply(impl_.symbol, Impls(impls)));
                        } else {
                            return None;
                        }
                    }
                }
                // no impl found
                // TODO: somehow give proper names to type variables
                let msg = format!(
                    "Type `{}` does not implement trait `{}`.",
                    t.display(self.symbol_names),
                    self.symbol_names[&trait_]);
                self.errors
                    .trait_error(&self.module)
                    .note(msg, span)
                    .done();
                None
            }
        }
    }

    fn check_expr(&mut self, expr: &mut Node<Expr>) {
        match expr.value {
            Expr::Apply(ref mut a, ref mut b) => {
                self.check_expr(&mut **a);
                self.check_expr(&mut **b);
            }
            Expr::Tuple(ref mut items) => {
                for item in items {
                    self.check_expr(item);
                }
            }
            Expr::Literal(_) => {}
            Expr::Lambda(_, ref mut expr) => {
                self.check_expr(&mut **expr);
            }
            Expr::Var(Symbol::Unknown, _, _) => {}
            Expr::Var(Symbol::Known(sym), ref type_, ref mut required_impls) => {
                // println!("names: {:?}", self.symbol_names);
                // println!("types: {:?}", self.symbol_types);
                let principal = &self.symbol_types[&sym];
                if let Some(bounds) = get_trait_bounds(&principal, type_) {
                    let mut impls = HashMap::new();
                    for bound in bounds {
                        if let Some(source) = self.solve_constraint(
                            bound.instantiated_to,
                            bound.trait_,
                            expr.span)
                        {
                            impls.insert((bound.var, bound.trait_), source);
                        } else {
                            return;
                        }
                    }
                    *required_impls = Impls(impls);
                }
            }
            Expr::Let(ref mut defs, ref mut expr) => {
                for def in defs {
                    self.check_def(&mut def.value);
                }
                self.check_expr(&mut **expr);
            }
            Expr::Case(ref mut expr, ref mut branches) => {
                self.check_expr(&mut **expr);
                for branch in branches {
                    self.check_expr(&mut branch.value.value);
                    if let Some(ref mut guard) = branch.value.guard {
                        self.check_expr(guard);
                    }
                }
            }
        }
    }

    fn check_def(&mut self, def: &mut Def) {
        let mut known_impls = self.known_impls.clone();
        for var in &def.scheme.vars {
            for &bound in &var.bounds {
                known_impls.insert((var.id, bound));
            }
        }
        let mut ctx = SolverCtx {
            impls: self.impls,
            errors: self.errors,
            known_impls: &known_impls,
            symbol_names: self.symbol_names,
            symbol_types: self.symbol_types,
            module: self.module.clone(),
        };
        ctx.check_expr(&mut def.value);
    }
}

struct GlobalSolver<'a, 'b> {
    impls: &'a [Impl],
    errors: &'b mut Errors,
    symbol_names: &'a HashMap<Sym, String>,
    symbol_types: &'a HashMap<Sym, Scheme>,
}

impl<'a, 'b> GlobalSolver<'a, 'b> {
    fn new(
            impls: &'a [Impl],
            errors: &'b mut Errors,
            symbol_names: &'a HashMap<Sym, String>,
            symbol_types: &'a HashMap<Sym, Scheme>) -> Self {
        GlobalSolver {
            impls,
            errors,
            symbol_names,
            symbol_types,
        }
    }

    fn check_def(&mut self, def: &mut Def) {
        let mut solver = SolverCtx {
            impls: self.impls,
            errors: self.errors,
            symbol_names: self.symbol_names,
            symbol_types: self.symbol_types,
            known_impls: &HashSet::new(),
            module: def.module.clone(),
        };

        solver.check_def(def);
    }

    fn check_impl(&mut self, impl_: &mut Impl) {
        let mut impl_bound = HashSet::new();
        for var in &impl_.scheme.value.vars {
            for &bound in &var.bounds {
                impl_bound.insert((var.id, bound));
            }
        }
        for def in &mut impl_.items {
            let known_impls = impl_bound
                .iter()
                .cloned()
                .map(|(var, trait_)| {
                    let var = def.var_mapping[&var];
                    (var, trait_)
                })
                .collect::<HashSet<_>>();
            let mut solver = SolverCtx {
                impls: self.impls,
                errors: self.errors,
                symbol_names: self.symbol_names,
                symbol_types: self.symbol_types,
                known_impls: &known_impls,
                module: impl_.module.clone(),
            };
            solver.check_def(&mut def.def);
        }
    }
}

pub fn check_items(items: &mut Items, errors: &mut Errors) {
    let checked_impls = {
        let mut solver = GlobalSolver::new(
            &items.impls,
            errors,
            &items.symbol_names,
            &items.symbol_types);
        
        for def in &mut items.items {
            solver.check_def(def);
        }

        let mut checked_impls = Vec::new();
        for impl_ in &items.impls {
            let mut impl_ = impl_.clone();
            solver.check_impl(&mut impl_);
            checked_impls.push(impl_);
        }

        checked_impls
    };

    items.impls = checked_impls;
}
