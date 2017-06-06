use position::Span;


#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Copy, Clone)]
pub struct NodeId(u64);

impl NodeId {
    pub fn new(id: u64) -> NodeId {
        NodeId(id)
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct Node<T> {
    pub node: T,
    pub span: Span,
    pub id: NodeId,
}

impl<T> Node<T> {
    pub fn new(node: T, span: Span) -> Node<T> {
        Node {
            node: node,
            span: span,
            id: NodeId::new(0),
        }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Node<U> {
        Node {
            node: f(self.node),
            span: self.span,
            id: self.id,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Char(char),
}

#[derive(PartialEq, Debug, Clone)]
pub enum Expr {
    Ident(String),
    Literal(Literal),
    Apply(Box<Node<Expr>>, Vec<Node<Expr>>),
    If(Box<Node<Expr>>, Box<Node<Expr>>, Box<Node<Expr>>),
    Infix(Box<Node<Expr>>, Node<String>, Box<Node<Expr>>),
    Parenthesised(Box<Node<Expr>>),
    Lambda(Vec<Node<Pattern>>, Box<Node<Expr>>),
    Case(Box<Node<Expr>>, Vec<Node<CaseBranch>>),
    Let(Vec<Node<LetDecl>>, Box<Node<Expr>>),
    Error,
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
    Error,
}

impl Pattern {
    pub fn from_symbol(symbol: String, span: Span) -> Pattern {
        if symbol.chars().nth(0).unwrap().is_uppercase() {
            Pattern::Deconstruct(Node::new(symbol, span), Vec::new())
        } else {
            Pattern::Var(symbol)
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum LetDecl {
    Def(Def),
    Type(TypeAnnot),
}

#[derive(PartialEq, Debug, Clone)]
pub struct Def {
    pub pattern: Node<Pattern>,
    pub value: Node<Expr>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypeAnnot {
    pub value: Node<String>,
    pub type_: Node<Scheme>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    SelfType,
    Var(String),
    Concrete(String),
    Apply(Box<Node<Type>>, Box<Node<Type>>),
    Function(Box<Node<Type>>, Box<Node<Type>>),
}

impl Type {
    pub fn from_symbol(symbol: String) -> Type {
        if symbol == "self" {
            Type::SelfType
        } else if symbol.chars().nth(0).unwrap().is_uppercase() {
            Type::Concrete(symbol)
        } else {
            Type::Var(symbol)
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Scheme {
    pub type_: Node<Type>,
    pub vars: Vec<(Node<String>, Node<Type>)>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Decl {
    Let(LetDecl),
    Infix(Associativity, Node<String>, Node<u64>),
    TypeAlias(TypeAlias),
    Union(UnionType),
    Record(RecordType),
    Trait(Trait),
}

#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
pub enum Associativity {
    Left,
    Right,
    None,
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypeAlias {
    pub name: Node<String>,
    pub vars: Vec<Node<String>>,
    pub type_: Node<Type>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct UnionType {
    pub name: Node<String>,
    pub vars: Vec<Node<String>>,
    pub cases: Vec<Node<UnionCase>>,
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
}

#[derive(PartialEq, Debug, Clone)]
pub struct Trait {
    pub name: Node<String>,
    pub vars: Vec<Node<String>>,
    pub values: Vec<Node<TypeAnnot>>,
    pub base_traits: Vec<Node<Type>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Impl {
    pub scheme: Scheme,
    pub trait_: Node<Type>,
    pub values: Vec<Node<Def>>,
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Kind {
    Star,
    Arrow(Box<Kind>, Box<Kind>),
}

// impl Display for Kind { }

/* ???
pub enum Symbol {
    Global { path: String, name: String },
    Local { name: String },
}

pub enum RawSymbol {
    Qualified(String, String),
    Unqualified(String),
    Trusted(String, String),
}

pub enum Symbol {
    Global(String, String),
    Local(String)
}

pub struct RawSymbol {
    pub name: String,
    pub path: Option<String>,
}
*/
