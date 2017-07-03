use std::fmt;
use ast::{Node, Literal};
use position::Span;


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
            Type::Apply(_) => 1,
            Type::Function(_) => 0,
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

#[derive(Debug, Clone)]
pub enum Expr {
    Literal(Literal),
    Var(Symbol, Type),
    Apply(Box<Node<Expr>>, Box<Node<Expr>>),
    Infix(Box<Node<Expr>>, Node<Symbol>, Type, Box<Node<Expr>>),
    Lambda(Vec<Node<Pattern>>, Box<Node<Expr>>),
    Let(Vec<(Node<Pattern>, Node<Expr>)>, Node<Expr>),
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
    Var(String),
    Deconstruct(Node<String>, Vec<Node<Pattern>>),
    Literal(Literal),
    As(Box<Node<Pattern>>, Node<String>),
    Tuple(Vec<Node<Pattern>>),
}

#[derive(Debug, Clone)]
pub enum Symbol {
    Local(String),
    Global(String),
    Unknown,
}

#[derive(Debug, Clone)]
pub struct Def {
    pub pattern: Node<Pattern>,
    pub expr: Node<Expr>,
    pub type_: Type,
    pub module: String,
}

#[derive(Debug, Clone)]
pub struct Record {
    pub name: Node<String>,
    pub vars: Vec<String>,
    pub fields: Vec<(Node<String>, Type)>,
    pub module: String,
}

#[derive(Debug, Clone)]
pub struct Union {
    pub name: Node<String>,
    pub vars: Vec<String>,
    pub cases: Vec<(Node<String>, Vec<Type>)>,
    pub module: String,
}

#[derive(Debug, Clone)]
pub struct Trait {
    pub name: Node<String>,
    pub items: Vec<Type>,
    pub module: String,
}

#[derive(Debug, Clone)]
pub struct Impl {
    pub type_: Node<Type>,
    pub trait_: Node<String>,
    pub items: Vec<Def>,
    pub module: String,
}
