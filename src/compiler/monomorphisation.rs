use std::collections::BTreeMap;
use ast::typed::{self as t, Sym, ImplSym, ImplSource, Impls};
use ast::monomorphised as m;

#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Copy, Clone)]
struct Context(u64);

const GLOBAL: Context = Context(0);

struct Solver {
    impls: BTreeMap<ImplSym, t::Impl>,
    defs: BTreeMap<(Sym, Context), (t::Def, Impls)>,
    instantiations: BTreeMap<(Sym, Context), BTreeMap<Impls, m::Def>>,
    current_context: BTreeMap<Sym, Context>,
    owner_trait: BTreeMap<Sym, Sym>,
    next_sym: u64,
    simple_defs: Vec<Sym>,
    next_context: u64,
    symbol_names: BTreeMap<Sym, String>,
}

impl Solver {
    fn from_items(items: t::Items) -> Solver {
        let mut impls = BTreeMap::new();
        for impl_ in items.impls {
            impls.insert(impl_.symbol, impl_);
        }
        let mut defs = BTreeMap::new();
        let mut simple_defs = Vec::new();
        for def in items.items {
            if !is_parametrised(&def) {
                simple_defs.push(def.sym.value);
            }
            defs.insert((def.sym.value, GLOBAL), (def, Default::default()));
        }
        let mut owner_trait = BTreeMap::new();
        for trait_ in items.traits {
            for (sym, _) in trait_.items {
                owner_trait.insert(sym, trait_.name.value);
            }
        }
        Solver {
            impls,
            defs,
            instantiations: BTreeMap::new(),
            owner_trait,
            next_sym: 0,
            simple_defs,
            next_context: 0,
            current_context: BTreeMap::new(),
            symbol_names: items.symbol_names,
        }
    }

    fn fresh_sym(&mut self, parent: Sym) -> Sym {
        self.next_sym += 1;
        let name = format!("{}_#{}", self.symbol_names[&parent], self.next_sym);
        let sym = Sym(self.next_sym + 1000000000);
        self.symbol_names.insert(sym, name);
        sym
    }

    fn create_context(&mut self) -> Context {
        self.next_context += 1;
        Context(self.next_context)
    }

    fn instantiate_def(&mut self, def: Sym, impls: &Impls) -> Sym {
        let ctx = *self.current_context.get(&def).unwrap_or(&GLOBAL);
        if let Some(def) = self.instantiations
                .entry((def, ctx))
                .or_insert_with(BTreeMap::new)
                .get(&impls) {
            return def.sym;
        }
        if !self.owner_trait.contains_key(&def) && !self.defs.contains_key(&(def, ctx)) {
            return def;
        }
        // don't rename the symbol for defs without var bounds -
        // we will have only one instantiation anyways
        // (also this does not fuck with builtin symbols)
        let sym = if impls.0.len() == 0 {
            def
        } else {
            self.fresh_sym(def)
        };
        let temp_def = m::Def {
            sym,
            value: m::Expr::Constructor(Sym(0), 0),
        };
        self.instantiations
            .get_mut(&(def, ctx))
            .unwrap()
            .insert(impls.clone(), temp_def);
        let instantiated = if self.owner_trait.contains_key(&def) {
            self.instantiate_trait_item(def, ctx, &impls)
        } else if self.defs.contains_key(&(def, ctx)) {
            self.instantiate_simple_def(def, ctx, &impls)
        } else {
            unreachable!()
        };
        self.instantiations
            .get_mut(&(def, ctx))
            .unwrap()
            .get_mut(&impls)
            .unwrap()
            .value = instantiated;
        sym
    }

    fn instantiate_trait_item(&mut self, def: Sym, _: Context, impls: &Impls) -> m::Expr {
        let trait_ = self.owner_trait[&def];
        match impls.0[&(0, trait_)] {
            ImplSource::FromContext(_, _) => {
                panic!("bad raw impl")
            }
            ImplSource::Apply(sym, ref params) => {
                let impl_ = self.impls[&sym].clone();
                let impl_def = impl_.get_impl_of(def).unwrap();
                let mut impls = impls.map_vars(&impl_def.var_mapping);
                let params = params.map_vars(&impl_def.var_mapping);
                impls.merge(params);
                self.instantiate_raw(&impl_def.def, &impls)
            }
            ImplSource::TupleEq(_) |
            ImplSource::TupleOrd(_) |
            ImplSource::TupleToString(_) => {
                panic!("tuple impls are not yet implemented")
            }
        }
    }

    fn instantiate_simple_def(&mut self, def: Sym, ctx: Context, impls: &Impls) -> m::Expr {
        let (def, mut base_impls) = self.defs[&(def, ctx)].clone();
        base_impls.merge(impls.clone());
        self.instantiate_raw(&def, &base_impls)
    }

    fn instantiate_raw(&mut self, def: &t::Def, impls: &Impls) -> m::Expr {
        self.instantiate_expr(&def.value.value, &impls)
    }

    fn instantiate_expr(&mut self, expr: &t::Expr, impls: &Impls) -> m::Expr {
        match *expr {
            t::Expr::Literal(ref lit) => {
                m::Expr::Literal(lit.clone())
            }
            t::Expr::Var(t::Symbol::Known(sym), _, ref impl_args) => {
                let sym = self.instantiate_symbol(sym, impl_args, impls);
                m::Expr::Var(sym)
            }
            t::Expr::Var(t::Symbol::Unknown, _, _) => {
                panic!("monomorphising code with unresolved symbols")
            }
            t::Expr::Apply(ref a, ref b) => {
                let a = self.instantiate_expr(&a.value, impls);
                let b = self.instantiate_expr(&b.value, impls);
                m::Expr::Apply(Box::new(a), vec![b])
            }
            t::Expr::Lambda(ref sym, ref value) => {
                let value = self.instantiate_expr(&value.value, impls);
                m::Expr::Lambda(vec![sym.value], Box::new(value))
            }
            t::Expr::Let(ref defs, ref value) => {
                let mut simple_defs = Vec::new();
                let mut prev_context = BTreeMap::new();
                let context = self.create_context();
                for def in defs {
                    if !is_parametrised(&def.value) {
                        simple_defs.push(def.value.sym.value);
                    }
                    let d = (def.value.clone(), impls.clone());
                    self.defs.insert((def.value.sym.value, context), d);
                    if let Some(&ctx) = self.current_context.get(&def.value.sym.value) {
                        prev_context.insert(def.value.sym.value, ctx);
                    }
                    self.current_context.insert(def.value.sym.value, context);
                }
                for def in simple_defs {
                    self.instantiate_def(def, impls);
                }
                let value = self.instantiate_expr(&value.value, impls);
                for (def, ctx) in prev_context {
                    self.current_context.insert(def, ctx);
                }
                let mut instantiations = Vec::new();
                for def in defs {
                    let key = (def.value.sym.value, context);
                    if let Some(inst) = self.instantiations.remove(&key) {
                        instantiations.extend(inst
                            .into_iter()
                            .map(|(_, def)| def));
                    }
                }
                m::Expr::Let(instantiations, Box::new(value))
            }
            t::Expr::Tuple(ref items) => {
                let items = items.iter().map(|item| {
                    self.instantiate_expr(&item.value, impls)
                }).collect::<Vec<_>>();
                let constructor = m::Expr::Constructor(Sym(1), items.len() as u64);
                m::Expr::Apply(Box::new(constructor), items)
            }
            t::Expr::Case(ref value, ref branches) => {
                let value = self.instantiate_expr(&value.value, impls);
                let mut m_branches = Vec::new();
                for branch in branches {
                    let pattern = self.instantiate_pattern(&branch.value.pattern.value);
                    let value = self.instantiate_expr(&branch.value.value.value, impls);
                    let guard = branch.value.guard.as_ref().map(|expr| {
                        self.instantiate_expr(&expr.value, impls)
                    });
                    m_branches.push(m::CaseBranch {
                        pattern,
                        value,
                        guard,
                    });
                }
                m::Expr::Case(Box::new(value), m_branches)
            }
        }
    }

    fn instantiate_symbol(
                            &mut self,
                            sym: Sym,
                            impls: &Impls,
                            context_impls: &Impls) -> Sym {
        let impls = impls.with_context(context_impls);
        self.instantiate_def(sym, &impls)
    }

    fn instantiate_pattern(&self, pattern: &t::Pattern) -> m::Pattern {
        match *pattern {
            t::Pattern::Wildcard => {
                m::Pattern::Wildcard
            }
            t::Pattern::Deconstruct(ref sym, ref items) => {
                let items = items.iter().map(|item| {
                    self.instantiate_pattern(&item.value)
                }).collect::<Vec<_>>();
                if let t::Symbol::Known(sym) = sym.value {
                    m::Pattern::Deconstruct(sym, items)
                } else {
                    panic!("monomorphising code with unresolved symbols")
                }
            }
            t::Pattern::Literal(ref lit) => {
                m::Pattern::Literal(lit.clone())
            }
            t::Pattern::As(ref pat, ref sym) => {
                let pat = self.instantiate_pattern(&pat.value);
                m::Pattern::As(Box::new(pat), sym.value)
            }
            t::Pattern::Tuple(ref items) => {
                let items = items.iter().map(|item| {
                    self.instantiate_pattern(&item.value)
                }).collect::<Vec<_>>();
                m::Pattern::Deconstruct(Sym(1), items)
            }
        }
    }

    fn run(mut self) -> (Vec<m::Def>, BTreeMap<Sym, String>) {
        let mut simple_defs = Vec::new();
        ::std::mem::swap(&mut simple_defs, &mut self.simple_defs);
        for def in simple_defs {
            self.instantiate_def(def, &Default::default());
        }
        let defs = self.instantiations
            .into_iter()
            .flat_map(|(_, m)| m.into_iter())
            .map(|(_, def)| def)
            .collect();
        (defs, self.symbol_names)
    }
}

fn is_parametrised(def: &t::Def) -> bool {
    for var in &def.scheme.vars {
        if var.bounds.len() > 0 {
            return true;
        }
    }
    false
}

fn make_constructors(types: &[t::TypeDecl]) -> Vec<m::Def> {
    let mut defs = Vec::new();
    for decl in types {
        match *decl {
            t::TypeDecl::Record(ref record) => {
                defs.push(m::Def {
                    sym: record.name.value,
                    value: m::Expr::Constructor(record.name.value, record.fields.len() as u64),
                });
                // TODO: make field getters
            }
            t::TypeDecl::Union(ref union) => {
                for &(ref sym, ref args) in &union.cases {
                    defs.push(m::Def {
                        sym: sym.value,
                        value: m::Expr::Constructor(sym.value, args.len() as u64),
                    });
                }
            }
        }
    }
    defs
}

pub fn monomorphise(items: t::Items) -> m::Items {
    let mut defs = make_constructors(&items.types);
    let ctx = Solver::from_items(items);
    let (defs2, symbol_names) = ctx.run();
    defs.extend(defs2);
    m::Items {
        items: defs,
        symbol_names,
    }
}
