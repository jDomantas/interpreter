use position::Span;


#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Copy, Clone)]
pub struct NodeId(u64);

impl NodeId {
    pub fn new(id: u64) -> NodeId {
        NodeId(id)
    }
}

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
}

pub enum Literal {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
    Char(char),
}

pub enum Expr {
    Ident(String),
    Literal(Literal),
    Apply(Box<Node<Expr>>, Vec<Node<Expr>>),
    If(Box<Node<Expr>>, Box<Node<Expr>>, Box<Node<Expr>>),
    Infix(Box<Node<Expr>>, Node<String>, Box<Node<Expr>>),
    Parenthesised(Box<Node<Expr>>),
    Error,
}

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

pub enum Symbol {
    Global { path: String, name: String },
    Local { name: String },
}

pub struct RawSymbol {
    pub name: String,
    pub path: Option<String>,
}
