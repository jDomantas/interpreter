pub mod parsed;
pub mod resolved;
pub mod typed;
pub mod monomorphised;

use codemap::Span;
pub use symbols::Sym;


#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Hash, Copy, Clone)]
pub enum Symbol {
    Known(Sym),
    Unknown,
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct Node<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Node<T> {
    pub fn new(value: T, span: Span) -> Node<T> {
        Node {
            value: value,
            span: span,
        }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Node<U> {
        Node {
            value: f(self.value),
            span: self.span,
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum Literal {
    Int(u64),
    Float(f64),
    Bool(bool),
    Str(String),
    Char(char),
}

#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
pub enum Associativity {
    Left,
    Right,
    None,
}

impl Associativity {
    pub fn as_str(self) -> &'static str {
        match self {
            Associativity::Left => "left-associative",
            Associativity::Right => "right-associative",
            Associativity::None => "non-associative",
        }
    }
}
