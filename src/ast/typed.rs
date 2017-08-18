use std::collections::{HashSet, HashMap};
use std::fmt;
use std::rc::Rc;
use ast::{Node, Name, Literal};
pub use ast::resolved::{Sym, Symbol};

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Type {
    Any,
    Var(u64),
    Concrete(Sym),
    Apply(Rc<Type>, Rc<Type>),
    Function(Rc<Type>, Rc<Type>),
    Tuple(Vec<Type>),
}

impl Type {
    fn precedence(&self) -> u32 {
        match *self {
            Type::Any |
            Type::Var(_) |
            Type::Tuple(_) |
            Type::Concrete(_) => 2,
            Type::Apply(_, _) => 1,
            Type::Function(_, _) => 0,
        }
    }

    pub fn map_vars(&self, substitution: &HashMap<u64, Type>) -> Type {
        match *self {
            Type::Any => Type::Any,
            Type::Var(v) => {
                if let Some(type_) = substitution.get(&v) {
                    type_.clone()
                } else {
                    Type::Var(v)
                }
            }
            Type::Tuple(ref items) => {
                let items = items.iter().map(|t| t.map_vars(substitution)).collect();
                Type::Tuple(items)
            }
            Type::Concrete(ref name) => {
                Type::Concrete(name.clone())
            }
            Type::Apply(ref a, ref b) => {
                let a = a.map_vars(substitution);
                let b = b.map_vars(substitution);
                Type::Apply(Rc::new(a), Rc::new(b))
            }
            Type::Function(ref a, ref b) => {
                let a = a.map_vars(substitution);
                let b = b.map_vars(substitution);
                Type::Function(Rc::new(a), Rc::new(b))
            }

        }
    }

    pub fn display<'a, 'b>(&'a self, symbol_names: &'b HashMap<Sym, String>) -> TypeFormatter<'a, 'b> {
        TypeFormatter {
            type_: self,
            symbol_names,
        }
    }

    pub fn contains_var(&self, var: u64) -> bool {
        match *self {
            Type::Var(v) => v == var,
            Type::Any |
            Type::Concrete(_) => false,
            Type::Apply(ref a, ref b) |
            Type::Function(ref a, ref b) => {
                a.contains_var(var) || b.contains_var(var)
            }
            Type::Tuple(ref items) => {
                items.iter().any(|i| i.contains_var(var))
            }
        }
    }

    pub fn collect_vars(&self, to: &mut HashSet<u64>) {
        match *self {
            Type::Any |
            Type::Concrete(_) => { }
            Type::Var(var) => {
                to.insert(var);
            }
            Type::Apply(ref a, ref b) |
            Type::Function(ref a, ref b) => {
                a.collect_vars(to);
                b.collect_vars(to);
            }
            Type::Tuple(ref items) => {
                for item in items {
                    item.collect_vars(to);
                }
            }
        }
    }
}

pub struct TypeFormatter<'a, 'b> {
    type_: &'a Type,
    symbol_names: &'b HashMap<Sym, String>,
}

impl<'a, 'b> fmt::Display for TypeFormatter<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.type_ {
            Type::Any => write!(f, "?"),
            Type::Var(index) => write!(f, "t{}", index),
            Type::Concrete(sym) => write!(f, "{}", self.symbol_names[&sym]),
            Type::Tuple(ref items) => {
                try!(write!(f, "({}", items[0].display(self.symbol_names)));
                for item in items.iter().skip(1) {
                    try!(write!(f, ", {}", item.display(self.symbol_names)));
                }
                write!(f, ")")
            }
            Type::Apply(ref a, ref b) => {
                if a.precedence() < 1 {
                    try!(write!(f, "({}) ", a.display(self.symbol_names)));
                } else {
                    try!(write!(f, "{} ", a.display(self.symbol_names)));
                }
                if b.precedence() <= 1 {
                    write!(f, "({})", b.display(self.symbol_names))
                } else {
                    write!(f, "{}", b.display(self.symbol_names))
                }
            }
            Type::Function(ref a, ref b) => {
                if a.precedence() == 0 {
                    write!(
                        f, 
                        "({}) -> {}",
                        a.display(self.symbol_names),
                        b.display(self.symbol_names))
                } else {
                    write!(
                        f,
                        "{} -> {}",
                        a.display(self.symbol_names),
                        b.display(self.symbol_names))
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal, Type),
    Var(Symbol, Type, Impls),
    Apply(Box<Node<Expr>>, Box<Node<Expr>>),
    And(Box<Node<Expr>>, Box<Node<Expr>>),
    Or(Box<Node<Expr>>, Box<Node<Expr>>),
    Lambda(Node<Sym>, Box<Node<Expr>>),
    Let(Vec<Node<Def>>, Box<Node<Expr>>),
    Tuple(Vec<Node<Expr>>),
    Case(Box<Node<Expr>>, Vec<Node<CaseBranch>>),
}

impl Expr {
    pub fn substitute_inner(&mut self, substitution: &HashMap<u64, Type>) {
        match *self {
            Expr::Literal(_, ref mut typ) => {
                *typ = typ.map_vars(substitution);
            }
            Expr::Var(_, ref mut typ, _) => {
                // TODO: also substitute in impl params?
                *typ = typ.map_vars(substitution);
            }
            Expr::Apply(ref mut a, ref mut b) |
            Expr::And(ref mut a, ref mut b) |
            Expr::Or(ref mut a, ref mut b) => {
                a.value.substitute_inner(substitution);
                b.value.substitute_inner(substitution);
            }
            Expr::Lambda(_, ref mut v) => {
                v.value.substitute_inner(substitution);
            }
            Expr::Let(ref mut defs, ref mut value) => {
                value.value.substitute_inner(substitution);
                for def in defs {
                    def.value.scheme.type_.map_vars(substitution);
                    def.value.value.value.substitute_inner(substitution);
                }
            }
            Expr::Tuple(ref mut items) => {
                for item in items {
                    item.value.substitute_inner(substitution);
                }
            }
            Expr::Case(ref mut expr, ref mut branches) => {
                expr.value.substitute_inner(substitution);
                for branch in branches {
                    branch.value.substitute_inner(substitution);
                }
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Impls(pub HashMap<(u64, Sym), ImplSource>);

impl Impls {
    pub fn empty() -> Impls {
        Default::default()
    }
}

#[derive(Debug, Clone)]
pub enum ImplSource {
    FromContext(u64, Sym),
    Apply(ImplSym, Impls),
    TupleEq(Vec<ImplSource>),
    TupleOrd(Vec<ImplSource>),
    TupleToString(Vec<ImplSource>),
}

#[derive(Debug, Clone)]
pub struct CaseBranch {
    pub pattern: Node<Pattern>,
    pub guard: Option<Node<Expr>>,
    pub value: Node<Expr>,
}

impl CaseBranch {
    fn substitute_inner(&mut self, substitution: &HashMap<u64, Type>) {
        self.pattern.value.substitute_inner(substitution);
        self.value.value.substitute_inner(substitution);
        if let Some(ref mut guard) = self.guard {
            guard.value.substitute_inner(substitution);
        }
    }
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Deconstruct(Node<Symbol>, Vec<Node<Pattern>>),
    Literal(Literal, Type),
    As(Box<Node<Pattern>>, Node<Sym>),
    Tuple(Vec<Node<Pattern>>),
}

impl Pattern {
    fn substitute_inner(&mut self, substitution: &HashMap<u64, Type>) {
        match *self {
            Pattern::Wildcard => { }
            Pattern::Deconstruct(_, ref mut items) |
            Pattern::Tuple(ref mut items) => {
                for item in items {
                    item.value.substitute_inner(substitution);
                }
            }
            Pattern::Literal(_, ref mut typ) => {
                *typ = typ.map_vars(substitution);
            }
            Pattern::As(ref mut pat, _) => {
                pat.value.substitute_inner(substitution);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Def {
    pub sym: Node<Sym>,
    pub value: Node<Expr>,
    pub scheme: Scheme,
    pub module: Name,
}

impl Def {
    pub fn substitute_inner(&mut self, substitution: &HashMap<u64, Type>) {
        self.scheme.type_ = self.scheme.type_.map_vars(substitution);
        self.value.value.substitute_inner(substitution);
    }
}

#[derive(Debug, Clone)]
pub struct SchemeVar {
    pub id: u64,
    pub bounds: Vec<Sym>,
}

#[derive(Debug, Clone)]
pub struct Scheme {
    pub vars: Vec<SchemeVar>,
    pub type_: Type,
}

impl Scheme {
    pub fn display<'a, 'b>(
                        &'a self,
                        symbol_names: &'b HashMap<Sym, String>) -> SchemeFormatter<'a, 'b> {
        SchemeFormatter {
            scheme: self,
            symbol_names,
        }
    }
}

pub struct SchemeFormatter<'a, 'b> {
    scheme: &'a Scheme,
    symbol_names: &'b HashMap<Sym, String>,
}

impl<'a, 'b> fmt::Display for SchemeFormatter<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        try!(write!(f, "["));
        let mut need_comma = false;
        for var in &self.scheme.vars {
            if need_comma {
                try!(write!(f, ", "));
            } else {
                need_comma = true;
            }
            try!(write!(f, "t{}", var.id));
            if !var.bounds.is_empty() {
                try!(write!(f, " : "));
                let mut need_plus = false;
                for bound in &var.bounds {
                    if need_plus {
                        try!(write!(f, " + "));
                    } else {
                        need_plus = true;
                    }
                    try!(write!(f, "{}", self.symbol_names[bound]));
                }
            }
        }
        write!(f, "] {}", self.scheme.type_.display(self.symbol_names))
    }
}

#[derive(Debug, Clone)]
pub struct Record {
    pub name: Node<Sym>,
    pub vars: Vec<u64>,
    pub fields: Vec<(Node<Sym>, Type)>,
    pub module: Name,
}

#[derive(Debug, Clone)]
pub struct Union {
    pub name: Node<Sym>,
    pub vars: Vec<u64>,
    pub cases: Vec<(Node<Sym>, Vec<Type>)>,
    pub module: Name,
}

#[derive(Debug, Clone)]
pub enum TypeDecl {
    Record(Record),
    Union(Union),
}

#[derive(Debug, Clone)]
pub struct Trait {
    pub name: Node<Sym>,
    pub base_traits: Vec<Sym>,
    pub items: Vec<(Sym, Scheme)>,
    pub module: Name,
}

#[derive(Debug, Clone)]
pub struct ImplDef {
    pub def: Def,
    // mapping from impl vars to def vars
    pub var_mapping: HashMap<u64, u64>,
}

#[derive(Debug, Clone)]
pub struct Impl {
    pub symbol: ImplSym,
    pub scheme: Node<Scheme>,
    pub trait_: Node<Symbol>,
    pub items: Vec<ImplDef>,
    // mapping from impl symbols to trait symbols
    pub trait_items: HashMap<Sym, Sym>,
    pub module: Name,
}

#[derive(Debug, Clone, Default)]
pub struct Items {
    pub types: Vec<TypeDecl>,
    pub items: Vec<Def>,
    pub traits: Vec<Trait>,
    pub impls: Vec<Impl>,
    pub symbol_names: HashMap<Sym, String>,
    pub symbol_types: HashMap<Sym, Scheme>,
}

impl Items {
    pub fn new() {
        Default::default()
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub struct ImplSym(pub u64);


pub mod printer {
    use super::*;
    pub fn print_items(items: &Items) {
        let mut printer = Printer {
            indent: 0,
            symbol_names: &items.symbol_names,
        };
        for typ in &items.types {
            match *typ {
                TypeDecl::Record(ref record) => {
                    if record.module.as_str() == "Main" {
                        printer.print_record(record);
                        println!("");
                    }
                }
                TypeDecl::Union(ref union) => {
                    if union.module.as_str() == "Main" {
                        printer.print_union(union);
                        println!("");
                    }
                }
            }
        }
        for def in &items.items {
            if def.module.as_str() == "Main" {
                printer.print_def(def);
                println!("");
            }
        }
        for impl_ in &items.impls {
            if impl_.module.as_str() == "Main" {
                printer.print_impl(impl_);
                println!("");
            }
        }
    }

    struct Printer<'a> {
        indent: usize,
        symbol_names: &'a HashMap<Sym, String>,
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
                    self.print_expr(&a.value);
                    print!(" ");
                    self.print_expr(&b.value);
                    print!(")");
                }
                Expr::Case(ref expr, ref branches) => {
                    print!("(case ");
                    self.print_expr(&expr.value);
                    print!(" of");
                    self.indent += 1;
                    for branch in branches {
                        println!("");
                        self.print_branch(&branch.value);
                    }
                    print!(")");
                    self.indent -= 1;
                }
                Expr::Var(sym, ref type_, _) => {
                    print!("(");
                    self.print_symbol(sym);
                    print!(" : {})", type_.display(self.symbol_names));
                }
                Expr::Lambda(ref sym, ref value) => {
                    print!("(\\ ");
                    self.print_sym(sym.value);
                    print!(" -> ");
                    self.print_expr(&value.value);
                    print!(")");
                }
                Expr::Let(ref defs, ref value) => {
                    println!("(let");
                    self.indent += 1;
                    for def in defs {
                        self.print_indent();
                        self.print_def(&def.value);
                    }
                    self.indent -= 1;
                    self.print_indent();
                    println!("in");
                    self.indent += 1;
                    self.print_indent();
                    self.print_expr(&value.value);
                    print!(")");
                    self.indent -= 1;
                }
                Expr::Literal(ref lit, ref type_) => {
                    print!("({:?} : {})", lit, type_.display(self.symbol_names));
                }
                Expr::Tuple(ref items) => {
                    print!("(");
                    let mut need_comma = false;
                    for item in items {
                        if need_comma { print!(", "); }
                        self.print_expr(&item.value);
                        need_comma = true;
                    }
                    print!(")");
                }
                Expr::And(ref a, ref b) => {
                    self.print_expr(&a.value);
                    print!(" && ");
                    self.print_expr(&b.value);
                }
                Expr::Or(ref a, ref b) => {
                    self.print_expr(&a.value);
                    print!(" || ");
                    self.print_expr(&b.value);
                }
            }
        }

        fn print_sym(&mut self, symbol: Sym) {
            self.print_symbol(Symbol::Known(symbol));
        }

        fn print_symbol(&mut self, symbol: Symbol) {
            match symbol {
                Symbol::Known(sym) => {
                    print!("{}", self.symbol_names[&sym]);
                }
                Symbol::Unknown => {
                    print!("?");
                }
            }
        }

        fn print_branch(&mut self, branch: &CaseBranch) {
            self.print_indent();
            self.print_pattern(&branch.pattern.value);
            if let Some(ref guard) = branch.guard {
                print!(" if ");
                self.print_expr(&guard.value);
            }
            println!(" ->");
            self.indent += 1;
            self.print_indent();
            self.print_expr(&branch.value.value);
            self.indent -= 1;
        }

        fn print_pattern(&mut self, pattern: &Pattern) {
            match *pattern {
                Pattern::As(ref pat, ref alias) => {
                    print!("(");
                    self.print_pattern(&pat.value);
                    print!(" as ");
                    self.print_sym(alias.value);
                    print!(")");
                }
                Pattern::Deconstruct(ref name, ref parts) => {
                    self.print_symbol(name.value);
                    for part in parts {
                        print!(" ");
                        self.print_pattern(&part.value);
                    }
                }
                Pattern::Literal(ref lit, ref type_) => {
                    print!("({:?} : {})", lit, type_.display(self.symbol_names));
                }
                Pattern::Tuple(ref items) => {
                    print!("(");
                    let mut need_comma = false;
                    for item in items {
                        if need_comma { print!(", "); }
                        self.print_pattern(&item.value);
                        need_comma = true;
                    }
                    print!(")");
                }
                Pattern::Wildcard => {
                    print!("_");
                }
            }
        }

        fn print_union(&mut self, union: &Union) {
            print!("type ");
            self.print_sym(union.name.value);
            for &var in &union.vars {
                print!(" t{}", var);
            }
            println!(" =");
            self.indent += 1;
            for &(ref tag, ref args) in &union.cases {
                self.print_indent();
                print!("| ");
                self.print_sym(tag.value);
                for typ in args {
                    print!(" {}", typ.display(self.symbol_names));
                }
                println!("");
            }
            self.indent -= 1;
        }

        fn print_record(&mut self, record: &Record) {
            print!("type ");
            self.print_sym(record.name.value);
            for var in &record.vars {
                print!(" t{}", var);
            }
            if record.fields.len() == 0 {
                println!(" = {{}}");
            } else {
                println!(" =");
                self.indent += 1;
                let mut first = true;
                for &(ref name, ref type_) in &record.fields {
                    self.print_indent();
                    if first { print!("{{ "); } else { print!(", "); }
                    self.print_sym(name.value);
                    println!(" : {}", type_.display(self.symbol_names));
                    first = false;
                }
                self.print_indent();
                println!("}}");
                self.indent -= 1;
            }
        }

        fn print_def(&mut self, def: &Def) {
            self.print_sym(def.sym.value);
            println!(": {} =", def.scheme.display(self.symbol_names));
            self.indent += 1;
            self.print_indent();
            self.print_expr(&def.value.value);
            println!("");
            self.indent -= 1;
        }

        fn print_impl(&mut self, impl_: &Impl) {
            println!("impl {}", impl_.scheme.value.display(self.symbol_names));
            self.indent += 1;
            for def in &impl_.items {
                self.print_indent();
                self.print_def(&def.def);
                println!("");
            }
            self.indent -= 1;
        }
    }
}
