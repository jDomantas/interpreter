use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use ast::{Node, Name, Literal};


#[derive(Debug, Clone)]
pub enum Type {
    Any,
    Var(u64),
    Concrete(String),
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
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Type::Any => write!(f, "?"),
            Type::Var(index) => write!(f, "t{}", index),
            Type::Concrete(ref name) => write!(f, "{}", name),
            Type::Tuple(ref items) => {
                try!(write!(f, "({}", items[0]));
                for item in items.iter().skip(1) {
                    try!(write!(f, ", {}", item));
                }
                write!(f, ")")
            }
            Type::Apply(ref a, ref b) => {
                if a.precedence() < 1 {
                    try!(write!(f, "({}) ", a));
                } else {
                    try!(write!(f, "{} ", a));
                }
                if b.precedence() <= 1 {
                    write!(f, "({})", b)
                } else {
                    write!(f, "{}", b)
                }
            }
            Type::Function(ref a, ref b) => {
                if a.precedence() == 0 {
                    write!(f, "({}) -> {}", a, b)
                } else {
                    write!(f, "{} -> {}", a, b)
                }
            }
        }
    }
}

impl Type {
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
}

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal, Type),
    Var(Symbol, Type),
    Apply(Box<Node<Expr>>, Box<Node<Expr>>),
    Infix(Box<Node<Expr>>, Node<Symbol>, Type, Box<Node<Expr>>),
    Lambda(Node<Sym>, Box<Node<Expr>>),
    Let(Vec<Def>, Box<Node<Expr>>),
    Tuple(Vec<Node<Expr>>),
    Case(Box<Node<Expr>>, Vec<Node<CaseBranch>>),
}

#[derive(Debug, Clone)]
pub struct CaseBranch {
    pub pattern: Node<Pattern>,
    pub guard: Option<Node<Expr>>,
    pub value: Node<Expr>,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Deconstruct(Node<Symbol>, Vec<Node<Pattern>>),
    Literal(Literal, Type),
    As(Box<Node<Pattern>>, Node<Sym>),
    Tuple(Vec<Node<Pattern>>),
}

pub use ast::resolved::{Sym, Symbol};

#[derive(Debug, Clone)]
pub struct Def {
    pub pattern: Node<Sym>,
    pub expr: Node<Expr>,
    pub scheme: Scheme,
    pub module: Name,
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

#[derive(Debug, Clone)]
pub struct Record {
    pub name: Node<Sym>,
    pub vars: Vec<Sym>,
    pub fields: Vec<(Node<Sym>, Type)>,
    pub module: Name,
}

#[derive(Debug, Clone)]
pub struct Union {
    pub name: Node<Sym>,
    pub vars: Vec<Sym>,
    pub cases: Vec<(Node<Sym>, Vec<Type>)>,
    pub module: Name,
}

#[derive(Debug, Clone)]
pub struct Trait {
    pub name: Node<Sym>,
    pub items: Vec<Type>,
    pub module: Name,
}

#[derive(Debug, Clone)]
pub struct Impl {
    pub type_: Node<Type>,
    pub trait_: Node<Symbol>,
    pub items: Vec<Def>,
    pub module: Name,
}
