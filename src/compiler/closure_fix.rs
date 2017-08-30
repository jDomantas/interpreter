use std::collections::{BTreeSet, BTreeMap};
use ast::monomorphised::{Expr, Pattern, CaseBranch, Sym, Def, Items};
use ast::monomorphised::rewriter::{self, Rewriter};


fn replace_with_inner<T, F>(value: &mut T, dummy: T, select: F)
    where F: FnOnce(&mut T) -> Option<&mut T>
{
    use std::mem::replace;
    if let Some(v) = select(value).map(|r| replace(r, dummy)) {
        *value = v;
    }
}

fn pattern_symbol(pattern: &Pattern) -> Option<Sym> {
    match *pattern {
        Pattern::As(ref pat, sym) => {
            match **pat {
                Pattern::Wildcard => Some(sym),
                _ => None,
            }
        }
        _ => None,
    }
}

fn value_symbol(expr: &Expr) -> Option<Sym> {
    match *expr {
        Expr::Var(sym) => Some(sym),
        _ => None,
    }
}

struct SimplifyMatching;

impl SimplifyMatching {
    fn can_remove_match(branches: &[CaseBranch]) -> bool {
        if branches.len() != 1 || branches[0].guard.is_some() {
            return false;
        }

        let pattern_symbol = pattern_symbol(&branches[0].pattern);
        let value_symbol = value_symbol(&branches[0].value);
        match (pattern_symbol, value_symbol) {
            (Some(a), Some(b)) if a == b => true,
            _ => false,
        }
    }
}

impl rewriter::Rewriter for SimplifyMatching {
    fn rewrite_expr(&mut self, expr: &mut Expr) {
        rewriter::walk_expr(self, expr);
        replace_with_inner(expr, Expr::Var(Sym(0)), |e| match *e {
            Expr::Case(ref mut value, ref mut branches) => {
                if Self::can_remove_match(branches) {
                    Some(&mut **value)
                } else {
                    None
                }
            }
            _ => None,
        });
    }
}

#[derive(Default)]
struct SimplifyRenames {
    renames: BTreeMap<Sym, Sym>,
}

impl SimplifyRenames {
    fn add_rename(&mut self, from: Sym, to: Sym) {
        let to = self.rename_sym(to);
        self.renames.insert(from, to);
    }

    fn rename_sym(&mut self, mut sym: Sym) -> Sym {
        while let Some(&other) = self.renames.get(&sym) {
            sym = other;
        }
        sym
    }
}

impl Rewriter for SimplifyRenames {
    fn rewrite_expr(&mut self, expr: &mut Expr) {
        replace_with_inner(expr, Expr::Var(Sym(0)), |e| match *e {
            Expr::Var(ref mut sym) => {
                *sym = self.rename_sym(*sym);
                None
            }
            Expr::Case(ref mut value, ref mut branches)
                if branches.len() == 1 && branches[0].guard.is_none() => {
                if let Some(matched) = value_symbol(&**value) {
                    if let Some(renamed_to) = pattern_symbol(&branches[0].pattern) {
                        self.add_rename(renamed_to, matched);
                        self.rewrite_expr(&mut branches[0].value);
                        Some(&mut branches[0].value)
                    } else if let Pattern::Wildcard = branches[0].pattern {
                        self.rewrite_expr(&mut branches[0].value);
                        Some(&mut branches[0].value)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => {
                None
            }
        });
        rewriter::walk_expr(self, expr);
    }
}

struct JoinLambdas;

impl JoinLambdas {
    fn get_inner<'a>(expr: &'a mut Expr, param_vec: &mut Vec<Sym>) -> &'a mut Expr {
        match expr {
            &mut Expr::Lambda(ref params, ref mut inner) => {
                param_vec.extend(params.iter().cloned());
                Self::get_inner(&mut **inner, param_vec)
            }
            other => other,
        }
    }
}

impl Rewriter for JoinLambdas {
    fn rewrite_expr(&mut self, expr: &mut Expr) {
        match *expr {
            Expr::Lambda(ref mut params, ref mut value) => {
                replace_with_inner(&mut **value, Expr::Var(Sym(0)), |e| {
                    Some(Self::get_inner(e, params))
                });
            }
            _ => {}
        }
        replace_with_inner(expr, Expr::Var(Sym(0)), |e| {
            match *e {
                Expr::Lambda(ref params, ref mut value) if params.len() == 0 => {
                    Some(&mut **value)
                }
                _ => {
                    None
                }
            }
        });
        rewriter::walk_expr(self, expr);
    }
}

struct JoinApplications;

impl JoinApplications {
    fn add_args(&mut self, expr: &mut Expr, mut args: Vec<Expr>) {
        match *expr {
            Expr::Apply(ref mut e, ref mut a) => {
                match **e {
                    Expr::Apply(_, _) => {
                        for e in a.drain(..) {
                            args.push(e);
                        }
                        self.add_args(&mut **e, args);
                    }
                    _ => {
                        a.extend(args.into_iter().rev());
                    }
                }
            }
            _ => {},
        }
    }
}

impl Rewriter for JoinApplications {
    fn rewrite_expr(&mut self, expr: &mut Expr) {
        self.add_args(expr, Vec::new());
        rewriter::walk_expr(self, expr);
    }
}

struct RemoveEmptyApplications;

impl RemoveEmptyApplications {
    fn get_next_inner(expr: &mut Expr) -> &mut Expr {
        match *expr {
            Expr::Apply(ref mut e, ref args) if args.len() == 0 => {
                Self::get_next_inner(&mut **e)
            }
            ref mut other => {
                other
            }
        }
    }
}

impl Rewriter for RemoveEmptyApplications {
    fn rewrite_expr(&mut self, expr: &mut Expr) {
        replace_with_inner(expr, Expr::Var(Sym(0)), |e| {
            Some(Self::get_next_inner(e))
        });
        rewriter::walk_expr(self, expr);
    }
}

struct Unclosure {
    next_sym: u64,
    symbol_names: BTreeMap<Sym, String>,
    globals: BTreeSet<Sym>,
    new_globals: Vec<Def>,
}

impl Unclosure {
    fn new() -> Self {
        Unclosure {
            next_sym: 2000000000,
            symbol_names: BTreeMap::new(),
            globals: BTreeSet::new(),
            new_globals: Vec::new(),
        }
    }

    fn fresh_sym(&mut self) -> Sym {
        self.next_sym += 1;
        let sym = Sym(self.next_sym);
        self.symbol_names.insert(sym, format!("$unc_{}", sym.0 - 2000000000));
        sym
    }
}

impl Rewriter for Unclosure {
    fn rewrite_expr(&mut self, expr: &mut Expr) {
        let extra_args = match *expr {
            Expr::Lambda(ref mut params, ref mut value) => {
                self.rewrite_expr(value);
                let mut free_vars = FreeVars::in_expr(value);
                for param in &*params {
                    free_vars.remove(param);
                }
                for global in &self.globals {
                    free_vars.remove(global);
                }
                if !free_vars.is_empty() {
                    let rename = free_vars
                        .into_iter()
                        .map(|sym| (sym, self.fresh_sym()))
                        .collect::<BTreeMap<_, _>>();
                    Renamer(&rename).rewrite_expr(value);
                    let rename = rename.into_iter().collect::<Vec<_>>();
                    let new_params = rename
                        .iter()
                        .map(|&(_, x)| x)
                        .chain(params.drain(..))
                        .collect::<Vec<_>>();
                    ::std::mem::replace(params, new_params);
                    rename.iter().map(|&(x, _)| Expr::Var(x)).collect::<Vec<_>>()
                } else {
                    return;
                }
            }
            Expr::Let(ref mut defs, ref mut value) => {
                self.rewrite_expr(value);
                let mut free_vars = FreeVars::default();
                for def in defs.iter_mut() {
                    free_vars.rewrite_def(def);
                }
                let mut free_vars = free_vars.vars;
                for global in &self.globals {
                    free_vars.remove(global);
                }
                let vars_before = free_vars.len();
                for def in defs.iter() {
                    free_vars.remove(&def.sym);
                }
                if free_vars.len() == vars_before {
                    // none of the defs are recursive
                    // so there is no need to fix closures
                    return;
                }
                if free_vars.len() == 0 {
                    // these defs don't close over anything
                    // just move them to globals
                    for def in defs.iter_mut() {
                        self.globals.insert(def.sym);
                    }
                    for mut def in defs.drain(..) {
                        self.rewrite_def(&mut def);
                        self.new_globals.push(def);
                    }
                    return;
                }
                // mapping from def symbols to new global symbols
                let base_renames = defs
                    .iter()
                    .map(|def| (def.sym, self.fresh_sym()))
                    .collect::<BTreeMap<_, _>>();
                let with_free_vars = base_renames
                    .iter()
                    .map(|(&s, &g)| {
                        let global = Box::new(Expr::Var(g));
                        let vars = free_vars.iter().map(|&v| Expr::Var(v)).collect();
                        (s, Expr::Apply(global, vars))
                    })
                    .collect::<BTreeMap<_, _>>();
                for def in defs.iter_mut() {
                    Replacer(&with_free_vars).rewrite_expr(&mut def.value);
                    self.globals.insert(base_renames[&def.sym]);
                }
                let mut new_let_defs = Vec::new();
                for mut def in defs.drain(..) {
                    // rename captured variables in each def to fresh ones
                    let rename = free_vars
                        .iter()
                        .map(|&sym| (sym, self.fresh_sym()))
                        .collect::<BTreeMap<_, _>>();
                    new_let_defs.push(Def {
                        sym: def.sym,
                        value: Expr::Apply(
                            Box::new(Expr::Var(base_renames[&def.sym])),
                            free_vars.iter().map(|&v| Expr::Var(v)).collect()),
                    });
                    def.sym = base_renames[&def.sym];
                    Renamer(&rename).rewrite_expr(&mut def.value);
                    // make def into a lambda
                    let old_val = ::std::mem::replace(&mut def.value, Expr::Var(Sym(0)));
                    let value = Expr::Lambda(rename.iter().map(|(_, &v)| v).collect(), Box::new(old_val));
                    def.value = value;
                    self.rewrite_def(&mut def);
                    self.new_globals.push(def);
                }
                *defs = new_let_defs;
                return;
            }
            _ => {
                rewriter::walk_expr(self, expr);
                return;
            }
        };
        *expr = Expr::Apply(Box::new(::std::mem::replace(expr, Expr::Var(Sym(0)))), extra_args);
    }

    fn rewrite_items(&mut self, items: &mut Items) {
        for def in &items.items {
            self.globals.insert(def.sym);
        }
        rewriter::walk_items(self, items);
        items.symbol_names.extend(::std::mem::replace(&mut self.symbol_names, BTreeMap::new()));
        items.items.extend(self.new_globals.drain(..));
    }
}

#[derive(Default)]
struct FreeVars {
    vars: BTreeSet<Sym>,
    bound_in_patterns: BTreeSet<Sym>,
}

impl FreeVars {
    fn in_expr(expr: &mut Expr) -> BTreeSet<Sym> {
        let mut free_vars = Self::default();
        free_vars.rewrite_expr(expr);
        free_vars.vars
    }
}

impl Rewriter for FreeVars {
    fn rewrite_expr(&mut self, expr: &mut Expr) {
        match *expr {
            Expr::Lambda(ref params, _) => {
                for &param in params {
                    self.vars.remove(&param);
                    self.bound_in_patterns.insert(param);
                }
            }
            Expr::Var(var) => {
                if !self.bound_in_patterns.contains(&var) {
                    self.vars.insert(var);
                }
            }
            Expr::Let(ref defs, _) => {
                for def in defs {
                    self.vars.remove(&def.sym);
                    self.bound_in_patterns.insert(def.sym);
                }
            }
            _ => {}
        }
        rewriter::walk_expr(self, expr);
    }

    fn rewrite_pattern(&mut self, pattern: &mut Pattern) {
        rewriter::walk_pattern(self, pattern);
        match *pattern {
            Pattern::As(_, var) => {
                self.vars.remove(&var);
                self.bound_in_patterns.insert(var);
            }
            _ => {}
        }
    }
}

struct Renamer<'a>(&'a BTreeMap<Sym, Sym>);

impl<'a> Rewriter for Renamer<'a> {
    fn rewrite_expr(&mut self, expr: &mut Expr) {
        match *expr {
            Expr::Var(ref mut sym) => {
                *sym = self.0.get(sym).cloned().unwrap_or(*sym);
            }
            ref mut e => {
                rewriter::walk_expr(self, e);
            }
        }
    }
}

struct Replacer<'a>(&'a BTreeMap<Sym, Expr>);

impl<'a> Rewriter for Replacer<'a> {
    fn rewrite_expr(&mut self, expr: &mut Expr) {
        let replace_with = match *expr {
            Expr::Var(sym) => {
                self.0.get(&sym).cloned().unwrap_or(Expr::Var(sym))
            }
            ref mut e => {
                rewriter::walk_expr(self, e);
                return;
            }
        };
        *expr = replace_with;
    }
}

struct RemoveEmptyLets;

impl RemoveEmptyLets {
    fn get_next_inner(expr: &mut Expr) -> &mut Expr {
        match expr {
            &mut Expr::Let(ref defs, ref mut e) if defs.len() == 0 => {
                Self::get_next_inner(e)
            }
            e => {
                e
            }
        }
    }
}

impl Rewriter for RemoveEmptyLets {
    fn rewrite_expr(&mut self, expr: &mut Expr) {
        replace_with_inner(expr, Expr::Var(Sym(0)), |e| {
            Some(Self::get_next_inner(e))
        });
        rewriter::walk_expr(self, expr);
    }
}

pub fn optimise(items: &mut Items) {
    SimplifyMatching.rewrite_items(items);
    SimplifyRenames::default().rewrite_items(items);
    JoinLambdas.rewrite_items(items);
    Unclosure::new().rewrite_items(items);
    RemoveEmptyLets.rewrite_items(items);
    JoinLambdas.rewrite_items(items);
    JoinApplications.rewrite_items(items);
    RemoveEmptyApplications.rewrite_items(items);
}
