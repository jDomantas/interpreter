use std::collections::HashMap;
use ast::{Node, Literal, Associativity};


#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Ident(Symbol),
    Literal(Literal),
    Apply(Box<Node<Expr>>, Box<Node<Expr>>),
    If(Box<Node<Expr>>, Box<Node<Expr>>, Box<Node<Expr>>),
    Infix(Box<Node<Expr>>, Node<Symbol>, Box<Node<Expr>>),
    Parenthesised(Box<Node<Expr>>),
    Lambda(Vec<Node<Pattern>>, Box<Node<Expr>>),
    Case(Box<Node<Expr>>, Vec<Node<CaseBranch>>),
    Let(Vec<Node<Def>>, Vec<Node<TypeAnnot>>, Box<Node<Expr>>),
    List(Vec<Node<Expr>>),
    Tuple(Vec<Node<Expr>>),
    Do(Box<Node<DoExpr>>),
}

#[derive(PartialEq, Debug, Clone)]
pub enum DoExpr {
    Done(Node<Expr>),
    Bind(Node<Pattern>, Node<Expr>, Box<Node<DoExpr>>),
    Sequence(Node<Expr>, Box<Node<DoExpr>>),
    If(Node<Expr>, Box<Node<DoExpr>>),
    Let(Node<Pattern>, Node<Expr>, Box<Node<DoExpr>>),
}

#[derive(PartialEq, Debug, Clone)]
pub struct CaseBranch {
    pub pattern: Node<Pattern>,
    pub value: Node<Expr>,
    pub guard: Option<Node<Expr>>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Var(String),
    Literal(Literal),
    Deconstruct(Node<String>, Vec<Node<Pattern>>),
    Infix(Box<Node<Pattern>>, Node<String>, Box<Node<Pattern>>),
    As(Box<Node<Pattern>>, Node<String>),
    Parenthesised(Box<Node<Pattern>>),
    Tuple(Vec<Node<Pattern>>),
    List(Vec<Node<Pattern>>),
}

#[derive(PartialEq, Debug, Clone)]
pub struct Def {
    pub pattern: Node<Pattern>,
    pub value: Option<Node<Expr>>,
    pub module: String,
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypeAnnot {
    pub value: Node<String>,
    pub type_: Option<Node<Scheme>>,
    pub module: String,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    Any,
    SelfType,
    Var(String),
    Concrete(String),
    Apply(Box<Node<Type>>, Box<Node<Type>>),
    Function(Box<Node<Type>>, Box<Node<Type>>),
    Tuple(Vec<Node<Type>>),
}

impl Type {
    pub fn contains_self(&self) -> bool {
        match *self {
            Type::Any => false,
            Type::SelfType => true,
            Type::Var(_) | Type::Concrete(_) => false,
            Type::Function(ref a, ref b) |
            Type::Apply(ref a, ref b) => {
                a.value.contains_self() || b.value.contains_self()
            }
            Type::Tuple(ref items) => {
                items.iter().any(|t| t.value.contains_self())
            }
        }
    }

    pub fn contains_var(&self, var: &str) -> bool {
        match *self {
            Type::Any => false,
            Type::Var(ref v) => var == v,
            Type::SelfType | Type::Concrete(_) => false,
            Type::Function(ref a, ref b) |
            Type::Apply(ref a, ref b) => {
                a.value.contains_var(var) || b.value.contains_var(var)
            }
            Type::Tuple(ref items) => {
                items.iter().any(|t| t.value.contains_var(var))
            }
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Scheme {
    pub type_: Node<Type>,
    pub bounds: Vec<(Node<String>, Node<String>)>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum TypeDecl {
    TypeAlias(TypeAlias),
    Union(UnionType),
    Record(RecordType),
}

impl TypeDecl {
    pub fn name(&self) -> &str {
        match *self {
            TypeDecl::Record(ref record) => &record.name.value,
            TypeDecl::TypeAlias(ref alias) => &alias.name.value,
            TypeDecl::Union(ref union) => &union.name.value,
        }
    }

    pub fn var_list(&self) -> &[Node<String>] {
        match *self {
            TypeDecl::Record(ref record) => &record.vars,
            TypeDecl::TypeAlias(ref alias) => &alias.vars,
            TypeDecl::Union(ref union) => &union.vars,
        }
    }

    pub fn module(&self) -> &str {
        match *self {
            TypeDecl::Record(ref record) => &record.module,
            TypeDecl::TypeAlias(ref alias) => &alias.module,
            TypeDecl::Union(ref union) => &union.module,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypeAlias {
    pub name: Node<String>,
    pub vars: Vec<Node<String>>,
    pub type_: Option<Node<Type>>,
    pub module: String,
}

#[derive(PartialEq, Debug, Clone)]
pub struct UnionType {
    pub name: Node<String>,
    pub vars: Vec<Node<String>>,
    pub cases: Vec<Node<UnionCase>>,
    pub module: String,
}

#[derive(PartialEq, Debug, Clone)]
pub struct UnionCase {
    pub tag: Node<String>,
    pub args: Vec<Node<Type>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct RecordType {
    pub name: Node<String>,
    pub vars: Vec<Node<String>>,
    pub fields: Vec<(Node<String>, Node<Type>)>,
    pub module: String,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Trait {
    pub name: Node<String>,
    pub base_traits: Vec<Node<String>>,
    pub values: Vec<Node<TypeAnnot>>,
    pub module: String,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Impl {
    pub scheme: Node<Scheme>,
    pub trait_: Node<String>,
    pub values: Vec<Node<Def>>,
    pub module: String,
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Symbol {
    Global(String),
    Local(String),
    Unknown,
}

impl Symbol {
    pub fn full_name(self) -> String {
        match self {
            Symbol::Global(name) | Symbol::Local(name) => name,
            Symbol::Unknown => "?".to_string(),
        }
    }

    pub fn full_name_ref(&self) -> &str {
        match *self {
            Symbol::Global(ref name) | Symbol::Local(ref name) => name,
            Symbol::Unknown => "?",
        }
    }
}

pub struct Items {
    pub types: Vec<TypeDecl>,
    pub items: Vec<Def>,
    pub traits: Vec<Trait>,
    pub impls: Vec<Impl>,
    pub annotations: HashMap<String, TypeAnnot>,
    pub fixities: HashMap<String, (Associativity, u64)>,
}

impl Items {
    pub fn empty() -> Items {
        Items {
            types: Vec::new(),
            items: Vec::new(),
            traits: Vec::new(),
            impls: Vec::new(),
            annotations: HashMap::new(),
            fixities: HashMap::new(),
        }
    }
}
