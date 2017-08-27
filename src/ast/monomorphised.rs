use std::collections::BTreeMap;
use ast::Literal;
pub use ast::resolved::Sym;


#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Var(Sym),
    Apply(Box<Expr>, Vec<Expr>),
    Lambda(Vec<Sym>, Box<Expr>),
    Let(Vec<Def>, Box<Expr>),
    Case(Box<Expr>, Vec<CaseBranch>),
    Constructor(Sym, u64),
}

#[derive(Debug, Clone)]
pub struct CaseBranch {
    pub pattern: Pattern,
    pub value: Expr,
    pub guard: Option<Expr>,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Deconstruct(Sym, Vec<Pattern>),
    Literal(Literal),
    As(Box<Pattern>, Sym),
}

#[derive(Debug, Clone)]
pub struct Def {
    pub sym: Sym,
    pub value: Expr,
}

#[derive(Debug, Clone)]
pub struct Items {
    pub items: Vec<Def>,
    pub symbol_names: BTreeMap<Sym, String>,
}


pub mod rewriter {
    use super::{Expr, Pattern, CaseBranch, Def, Items};

    
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


pub mod printer {
    use super::*;
    pub fn print_items(items: &Items) {
        let mut printer = Printer {
            indent: 0,
            symbol_names: &items.symbol_names,
        };
        for def in &items.items {
            printer.print_def(def);
            println!("");
        }
    }

    pub fn print_expr(symbol_names: &BTreeMap<Sym, String>, expr: &Expr) {
        let mut printer = Printer {
            indent: 0,
            symbol_names: &symbol_names,
        };
        printer.print_expr(expr);
    }

    struct Printer<'a> {
        indent: usize,
        symbol_names: &'a BTreeMap<Sym, String>,
    }

    impl<'a> Printer<'a> {
        fn print_indent(&self) {
            for _ in 0..(self.indent) {
                print!("  ");
            }
        }

        fn print_expr(&mut self, expr: &Expr) {
            match *expr {
                Expr::Apply(ref a, ref b) => {
                    print!("(");
                    self.print_expr(&**a);
                    for expr in b {
                        print!(" ");
                        self.print_expr(expr);
                    }
                    print!(")");
                }
                Expr::Case(ref expr, ref branches) => {
                    print!("(case ");
                    self.print_expr(&**expr);
                    print!(" of");
                    self.indent += 1;
                    for branch in branches {
                        println!("");
                        self.print_branch(&branch);
                    }
                    print!(")");
                    self.indent -= 1;
                }
                Expr::Var(sym) => {
                    self.print_sym(sym);
                }
                Expr::Lambda(ref sym, ref value) => {
                    print!("(\\");
                    for sym in sym {
                        print!(" ");
                        self.print_sym(*sym);
                    }
                    print!(" -> ");
                    self.print_expr(&**value);
                    print!(")");
                }
                Expr::Let(ref defs, ref value) => {
                    println!("(let");
                    self.indent += 1;
                    for def in defs {
                        self.print_indent();
                        self.print_def(def);
                    }
                    self.indent -= 1;
                    self.print_indent();
                    println!("in");
                    self.indent += 1;
                    self.print_indent();
                    self.print_expr(&**value);
                    print!(")");
                    self.indent -= 1;
                }
                Expr::Literal(ref lit) => {
                    print!("{:?}", lit);
                }
                Expr::Constructor(name, arg_count) => {
                    print!("ctor{{");
                    self.print_sym(name);
                    print!("}}{{{}}}", arg_count);
                }
            }
        }

        fn print_sym(&mut self, sym: Sym) {
            print!("{}", self.symbol_names[&sym]);
        }

        fn print_branch(&mut self, branch: &CaseBranch) {
            self.print_indent();
            self.print_pattern(&branch.pattern);
            if let Some(ref guard) = branch.guard {
                print!(" if ");
                self.print_expr(&guard);
            }
            println!(" ->");
            self.indent += 1;
            self.print_indent();
            self.print_expr(&branch.value);
            self.indent -= 1;
        }

        fn print_pattern(&mut self, pattern: &Pattern) {
            match *pattern {
                Pattern::As(ref pat, alias) => {
                    print!("(");
                    self.print_pattern(&**pat);
                    print!(" as ");
                    self.print_sym(alias);
                    print!(")");
                }
                Pattern::Deconstruct(name, ref parts) => {
                    print!("(");
                    self.print_sym(name);
                    for part in parts {
                        print!(" ");
                        self.print_pattern(part);
                    }
                    print!(")");
                }
                Pattern::Literal(ref lit) => {
                    print!("{:?}", lit);
                }
                Pattern::Wildcard => {
                    print!("_");
                }
            }
        }

        fn print_def(&mut self, def: &Def) {
            self.print_sym(def.sym);
            println!(" =");
            self.indent += 1;
            self.print_indent();
            self.print_expr(&def.value);
            println!("");
            self.indent -= 1;
        }
    }
}
