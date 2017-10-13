use std::collections::{BTreeSet, BTreeMap};
use std::fmt;
use std::rc::Rc;
use ast::{Node, Literal, Sym, Symbol};
use symbols::SymbolSource;


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

    pub fn map_vars(&self, substitution: &BTreeMap<u64, Type>) -> Type {
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

    pub fn display<'a>(&'a self, symbols: &'a SymbolSource) -> TypeFormatter<'a> {
        TypeFormatter {
            type_: self,
            symbols,
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

    pub fn collect_vars(&self, to: &mut BTreeSet<u64>) {
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

pub struct TypeFormatter<'a> {
    type_: &'a Type,
    symbols: &'a SymbolSource,
}

impl<'a> fmt::Display for TypeFormatter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.type_ {
            Type::Any => write!(f, "?"),
            Type::Var(index) => write!(f, "t{}", index),
            Type::Concrete(sym) => write!(f, "{}", self.symbols.symbol_name(sym)),
            Type::Tuple(ref items) if items.len() == 0 => {
                write!(f, "()")
            }
            Type::Tuple(ref items) => {
                try!(write!(f, "({}", items[0].display(self.symbols)));
                for item in items.iter().skip(1) {
                    try!(write!(f, ", {}", item.display(self.symbols)));
                }
                write!(f, ")")
            }
            Type::Apply(ref a, ref b) => {
                if a.precedence() < 1 {
                    try!(write!(f, "({}) ", a.display(self.symbols)));
                } else {
                    try!(write!(f, "{} ", a.display(self.symbols)));
                }
                if b.precedence() <= 1 {
                    write!(f, "({})", b.display(self.symbols))
                } else {
                    write!(f, "{}", b.display(self.symbols))
                }
            }
            Type::Function(ref a, ref b) => {
                if a.precedence() == 0 {
                    write!(
                        f, 
                        "({}) -> {}",
                        a.display(self.symbols),
                        b.display(self.symbols))
                } else {
                    write!(
                        f,
                        "{} -> {}",
                        a.display(self.symbols),
                        b.display(self.symbols))
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Var(Symbol, Type, Impls),
    Apply(Box<Node<Expr>>, Box<Node<Expr>>),
    Lambda(Node<Sym>, Box<Node<Expr>>),
    Let(Vec<Def>, Box<Node<Expr>>),
    Tuple(Vec<Node<Expr>>),
    Case(Box<Node<Expr>>, Vec<Node<CaseBranch>>),
}

impl Expr {
    pub fn substitute_inner(&mut self, substitution: &BTreeMap<u64, Type>) {
        match *self {
            Expr::Literal(_) => {}
            Expr::Var(_, ref mut typ, _) => {
                // TODO: also substitute in impl params?
                *typ = typ.map_vars(substitution);
            }
            Expr::Apply(ref mut a, ref mut b) => {
                a.value.substitute_inner(substitution);
                b.value.substitute_inner(substitution);
            }
            Expr::Lambda(_, ref mut v) => {
                v.value.substitute_inner(substitution);
            }
            Expr::Let(ref mut defs, ref mut value) => {
                value.value.substitute_inner(substitution);
                for def in defs {
                    def.scheme.type_.map_vars(substitution);
                    def.value.value.substitute_inner(substitution);
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

#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Hash, Clone, Default)]
pub struct Impls(pub BTreeMap<(u64, Sym), ImplSource>);

impl Impls {
    pub fn empty() -> Impls {
        Default::default()
    }

    pub fn merge(&mut self, other: Impls) {
        self.0.extend(other.0);
    }

    pub fn with_context(&self, ctx: &Impls) -> Impls {
        let mapping = self.0
            .iter()
            .map(|(k, v)| (k.clone(), v.with_context(ctx)))
            .collect();
        Impls(mapping)
    }

    pub fn map_vars(&self, mapping: &BTreeMap<u64, u64>) -> Impls {
        let impls = self.0
            .iter()
            .map(|(&(var, sym), src)| {
                let var = mapping.get(&var).cloned().unwrap_or(var);
                ((var, sym), src.map_vars(mapping))
            })
            .collect();
        Impls(impls)
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Hash, Clone)]
pub enum ImplSource {
    FromContext(u64, Sym),
    Apply(ImplSym, Impls),
    TupleEq(Vec<ImplSource>),
    TupleOrd(Vec<ImplSource>),
    TupleToString(Vec<ImplSource>),
}

impl ImplSource {
    fn with_context(&self, ctx: &Impls) -> ImplSource {
        match *self {
            ImplSource::FromContext(var, trait_) => {
                ctx.0[&(var, trait_)].clone()
            }
            ImplSource::Apply(sym, ref impls) => {
                let impls = impls.with_context(ctx);
                ImplSource::Apply(sym, impls)
            }
            ImplSource::TupleEq(ref items) => {
                let items = items.iter().map(|i|
                    i.with_context(ctx)
                ).collect();
                ImplSource::TupleEq(items)
            }
            ImplSource::TupleOrd(ref items) => {
                let items = items.iter().map(|i|
                    i.with_context(ctx)
                ).collect();
                ImplSource::TupleOrd(items)
            }
            ImplSource::TupleToString(ref items) => {
                let items = items.iter().map(|i|
                    i.with_context(ctx)
                ).collect();
                ImplSource::TupleToString(items)
            }
        }
    }

    fn map_vars(&self, var_mapping: &BTreeMap<u64, u64>) -> ImplSource {
        match *self {
            ImplSource::FromContext(var, trait_) => {
                ImplSource::FromContext(var, trait_)
            }
            ImplSource::Apply(sym, ref impls) => {
                let impls = impls.map_vars(var_mapping);
                ImplSource::Apply(sym, impls)
            }
            ImplSource::TupleEq(ref items) => {
                let items = items.iter().map(|i|
                    i.map_vars(var_mapping)
                ).collect();
                ImplSource::TupleEq(items)
            }
            ImplSource::TupleOrd(ref items) => {
                let items = items.iter().map(|i|
                    i.map_vars(var_mapping)
                ).collect();
                ImplSource::TupleOrd(items)
            }
            ImplSource::TupleToString(ref items) => {
                let items = items.iter().map(|i|
                    i.map_vars(var_mapping)
                ).collect();
                ImplSource::TupleToString(items)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct CaseBranch {
    pub pattern: Node<Pattern>,
    pub guard: Option<Node<Expr>>,
    pub value: Node<Expr>,
}

impl CaseBranch {
    fn substitute_inner(&mut self, substitution: &BTreeMap<u64, Type>) {
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
    Literal(Literal),
    As(Box<Node<Pattern>>, Node<Sym>),
    Tuple(Vec<Node<Pattern>>),
}

impl Pattern {
    fn substitute_inner(&mut self, substitution: &BTreeMap<u64, Type>) {
        match *self {
            Pattern::Wildcard => { }
            Pattern::Deconstruct(_, ref mut items) |
            Pattern::Tuple(ref mut items) => {
                for item in items {
                    item.value.substitute_inner(substitution);
                }
            }
            Pattern::Literal(_) => {}
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
    pub fn display<'a>(&'a self, symbols: &'a SymbolSource) -> SchemeFormatter<'a> {
        SchemeFormatter {
            scheme: self,
            symbols,
        }
    }
}

pub struct SchemeFormatter<'a> {
    scheme: &'a Scheme,
    symbols: &'a SymbolSource,
}

impl<'a> fmt::Display for SchemeFormatter<'a> {
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
                for &bound in &var.bounds {
                    if need_plus {
                        try!(write!(f, " + "));
                    } else {
                        need_plus = true;
                    }
                    try!(write!(f, "{}", self.symbols.symbol_name(bound)));
                }
            }
        }
        write!(f, "] {}", self.scheme.type_.display(self.symbols))
    }
}

#[derive(Debug, Clone)]
pub struct Union {
    pub name: Node<Sym>,
    pub vars: Vec<u64>,
    pub cases: Vec<(Node<Sym>, Vec<Type>)>,
}

#[derive(Debug, Clone)]
pub enum TypeDecl {
    Union(Union),
}

#[derive(Debug, Clone)]
pub struct Trait {
    pub name: Node<Sym>,
    pub base_traits: Vec<Sym>,
    pub items: Vec<(Sym, Scheme)>,
}

#[derive(Debug, Clone)]
pub struct ImplDef {
    pub def: Def,
    // mapping from impl vars to def vars
    pub var_mapping: BTreeMap<u64, u64>,
}

#[derive(Debug, Clone)]
pub struct Impl {
    pub symbol: ImplSym,
    pub scheme: Node<Scheme>,
    pub trait_: Node<Symbol>,
    pub items: Vec<ImplDef>,
    // mapping from impl symbols to trait symbols
    pub trait_items: BTreeMap<Sym, Sym>,
}

impl Impl {
    pub fn get_impl_of(&self, trait_sym: Sym) -> Option<&ImplDef> {
        self.trait_items
            .iter()
            .find(|&(_, v)| *v == trait_sym)
            .and_then(|(sym, _)| {
                self.items.iter().find(|d| d.def.sym.value == *sym)
            })
    }
}

#[derive(Debug, Clone, Default)]
pub struct Items {
    pub types: Vec<TypeDecl>,
    pub items: Vec<Def>,
    pub traits: Vec<Trait>,
    pub impls: Vec<Impl>,
    pub symbol_types: BTreeMap<Sym, Scheme>,
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Hash, Copy, Clone)]
pub struct ImplSym(pub u64);


pub mod printer {
    use super::*;
    use symbols::SymbolSource;


    #[allow(dead_code)]
    pub fn print_items(items: &Items, symbols: &SymbolSource) {
        let mut printer = Printer {
            indent: 0,
            symbols,
        };
        for &TypeDecl::Union(ref union) in &items.types {
            printer.print_union(union);
            println!("");
        }
        for def in &items.items {
            printer.print_def(def);
            println!("");
        }
        for impl_ in &items.impls {
            printer.print_impl(impl_);
            println!("");
        }
        for (&sym, typ) in &items.symbol_types {
            println!("symbol type:  {} ({:?}) : {}",
                symbols.symbol_name(sym),
                sym,
                typ.display(&symbols));
        }
    }

    struct Printer<'a> {
        indent: usize,
        symbols: &'a SymbolSource,
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
                    print!(" : {})", type_.display(self.symbols));
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
                        self.print_def(def);
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
                Expr::Literal(ref lit) => {
                    print!("{:?}", lit);
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
            }
        }

        fn print_sym(&mut self, symbol: Sym) {
            self.print_symbol(Symbol::Known(symbol));
        }

        fn print_symbol(&mut self, symbol: Symbol) {
            match symbol {
                Symbol::Known(sym) => {
                    print!("{}", self.symbols.symbol_name(sym));
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
                Pattern::Literal(ref lit) => {
                    print!("{:?}", lit);
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
                    print!(" {}", typ.display(self.symbols));
                }
                println!("");
            }
            self.indent -= 1;
        }

        fn print_def(&mut self, def: &Def) {
            self.print_sym(def.sym.value);
            println!(": {} =", def.scheme.display(self.symbols));
            self.indent += 1;
            self.print_indent();
            self.print_expr(&def.value.value);
            println!("");
            self.indent -= 1;
        }

        fn print_impl(&mut self, impl_: &Impl) {
            println!("impl {}", impl_.scheme.value.display(self.symbols));
            self.indent += 1;
            for def in &impl_.items {
                self.print_indent();
                print!("(var map: {:?}) ", def.var_mapping);
                self.print_def(&def.def);
                println!("");
            }
            self.indent -= 1;
        }
    }
}
