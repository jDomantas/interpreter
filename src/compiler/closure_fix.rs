use std::collections::{HashSet, HashMap};
use ast::monomorphised::{Expr, Pattern, CaseBranch, Sym, Def, Items};


mod rewriter {
    use ast::monomorphised::{Expr, Pattern, CaseBranch, Def, Items};

    
    pub trait Rewriter {
        fn rewrite_def(&mut self, def: &mut Def) {
            walk_def(self, def);
        }

        fn rewrite_expr(&mut self, expr: &mut Expr) {
            walk_expr(self, expr);
        }

        fn rewrite_branch(&mut self, branch: &mut CaseBranch) {
            walk_branch(self, branch);
        }

        fn rewrite_pattern(&mut self, pattern: &mut Pattern) {
            walk_pattern(self, pattern);
        }

        fn rewrite_items(&mut self, items: &mut Items) {
            walk_items(self, items);
        }
    }

    pub fn walk_def<R: Rewriter + ?Sized>(r: &mut R, def: &mut Def) {
        r.rewrite_expr(&mut def.value);
    }

    pub fn walk_expr<R: Rewriter + ?Sized>(r: &mut R, expr: &mut Expr) {
        match *expr {
            Expr::Literal(_) |
            Expr::Var(_) |
            Expr::Constructor(_, _) => {}
            Expr::Apply(ref mut f, ref mut args) => {
                r.rewrite_expr(&mut **f);
                for arg in args {
                    r.rewrite_expr(arg);
                }
            }
            Expr::Lambda(_, ref mut expr) => {
                r.rewrite_expr(&mut **expr);
            }
            Expr::Let(ref mut defs, ref mut value) => {
                for def in defs {
                    r.rewrite_def(def);
                }
                r.rewrite_expr(&mut **value);
            }
            Expr::Case(ref mut value, ref mut branches) => {
                r.rewrite_expr(&mut **value);
                for branch in branches {
                    r.rewrite_branch(branch);
                }
            }
        }
    }

    pub fn walk_branch<R: Rewriter + ?Sized>(r: &mut R, branch: &mut CaseBranch) {
        r.rewrite_pattern(&mut branch.pattern);
        if let Some(ref mut guard) = branch.guard {
            r.rewrite_expr(guard);
        }
        r.rewrite_expr(&mut branch.value);
    }

    pub fn walk_pattern<R: Rewriter + ?Sized>(r: &mut R, pattern: &mut Pattern) {
        match *pattern {
            Pattern::Wildcard |
            Pattern::Literal(_) => {}
            Pattern::Deconstruct(_, ref mut items) => {
                for item in items {
                    r.rewrite_pattern(item);
                }
            }
            Pattern::As(ref mut pat, _) => {
                r.rewrite_pattern(&mut **pat);
            }
        }
    }

    pub fn walk_items<R: Rewriter + ?Sized>(r: &mut R, items: &mut Items) {
        for def in &mut items.items {
            r.rewrite_def(def);
        }
    }
}

use self::rewriter::Rewriter;

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
    renames: HashMap<Sym, Sym>,
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
    symbol_names: HashMap<Sym, String>,
    globals: HashSet<Sym>,
    new_globals: Vec<Def>,
}

impl Unclosure {
    fn new() -> Self {
        Unclosure {
            next_sym: 2000000000,
            symbol_names: HashMap::new(),
            globals: HashSet::new(),
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
                        .collect::<HashMap<_, _>>();
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
                for def in defs.iter() {
                    free_vars.remove(&def.sym);
                }
                let base_renames = defs
                    .iter()
                    .map(|def| (def.sym, self.fresh_sym()))
                    .collect::<HashMap<_, _>>();
                for def in defs.iter_mut() {
                    Renamer(&base_renames).rewrite_expr(&mut def.value);
                    self.globals.insert(base_renames[&def.sym]);
                }
                for def in defs.iter_mut() {
                    self.rewrite_def(def);
                }
                let mut new_let_defs = Vec::new();
                for mut def in defs.drain(..) {
                    let rename = free_vars
                        .iter()
                        .map(|&sym| (sym, self.fresh_sym()))
                        .collect::<HashMap<_, _>>();
                    new_let_defs.push(Def {
                        sym: def.sym,
                        value: Expr::Apply(
                            Box::new(Expr::Var(base_renames[&def.sym])),
                            free_vars.iter().map(|&v| Expr::Var(v)).collect()),
                    });
                    def.sym = base_renames[&def.sym];
                    Renamer(&rename).rewrite_expr(&mut def.value);
                    self.globals.insert(def.sym);
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
        items.symbol_names.extend(self.symbol_names.drain());
        items.items.extend(self.new_globals.drain(..));
    }
}

#[derive(Default)]
struct FreeVars {
    vars: HashSet<Sym>,
    bound_in_patterns: HashSet<Sym>,
}

impl FreeVars {
    fn in_expr(expr: &mut Expr) -> HashSet<Sym> {
        let mut free_vars = Self::default();
        free_vars.rewrite_expr(expr);
        free_vars.vars
    }
}

impl Rewriter for FreeVars {
    fn rewrite_expr(&mut self, expr: &mut Expr) {
        match *expr {
            Expr::Lambda(_, _) => {
                return;
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

struct Renamer<'a>(&'a HashMap<Sym, Sym>);

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

pub fn optimise(items: &mut Items) {
    SimplifyMatching.rewrite_items(items);
    JoinLambdas.rewrite_items(items);
    SimplifyRenames::default().rewrite_items(items);
    Unclosure::new().rewrite_items(items);
    JoinApplications.rewrite_items(items);
    RemoveEmptyApplications.rewrite_items(items);
}
