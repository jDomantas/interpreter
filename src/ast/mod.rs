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
pub enum Expr<Sym> {
    Ident(Sym),
    Literal(Literal),
    Apply(Box<Node<Expr<Sym>>>, Vec<Node<Expr<Sym>>>),
    If(Box<Node<Expr<Sym>>>, Box<Node<Expr<Sym>>>, Box<Node<Expr<Sym>>>),
    Infix(Box<Node<Expr<Sym>>>, Node<Sym>, Box<Node<Expr<Sym>>>),
    Parenthesised(Box<Node<Expr<Sym>>>),
    Lambda(Vec<Node<Pattern<Sym>>>, Box<Node<Expr<Sym>>>),
    Case(Box<Node<Expr<Sym>>>, Vec<Node<CaseBranch<Sym>>>),
    Let(Vec<Node<LetDecl<Sym>>>, Box<Node<Expr<Sym>>>),
    Error,
}

#[derive(PartialEq, Debug, Clone)]
pub struct CaseBranch<Sym> {
    pub pattern: Node<Pattern<Sym>>,
    pub value: Node<Expr<Sym>>,
    pub guard: Option<Node<Expr<Sym>>>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Pattern<Sym> {
    Wildcard,
    Var(String),
    Literal(Literal),
    Deconstruct(Node<Sym>, Vec<Node<Pattern<Sym>>>),
    Infix(Box<Node<Pattern<Sym>>>, Node<Sym>, Box<Node<Pattern<Sym>>>),
    As(Box<Node<Pattern<Sym>>>, Node<String>),
    Parenthesised(Box<Node<Pattern<Sym>>>),
    Error,
}

impl Pattern<RawSymbol> {
    pub fn from_symbol(symbol: RawSymbol, span: Span) -> Pattern<RawSymbol> {
        match symbol {
            RawSymbol::Qualified(_, _) |
            RawSymbol::Trusted(_, _) => {
                Pattern::Deconstruct(Node::new(symbol, span), Vec::new())
            }
            RawSymbol::Unqualified(name) => {
                if name.chars().nth(0).unwrap().is_uppercase() {
                    Pattern::Deconstruct(Node::new(RawSymbol::Unqualified(name), span), Vec::new())
                } else {
                    Pattern::Var(name)
                }
            }
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum LetDecl<Sym> {
    Def(Def<Sym>),
    Type(TypeAnnot<Sym>),
}

#[derive(PartialEq, Debug, Clone)]
pub struct Def<Sym> {
    pub pattern: Node<Pattern<Sym>>,
    pub value: Node<Expr<Sym>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypeAnnot<Sym> {
    pub value: Node<String>,
    pub type_: Node<Scheme<Sym>>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Type<Sym> {
    SelfType,
    Var(String),
    Concrete(Sym),
    Apply(Box<Node<Type<Sym>>>, Box<Node<Type<Sym>>>),
    Function(Box<Node<Type<Sym>>>, Box<Node<Type<Sym>>>),
}

impl Type<RawSymbol> {
    pub fn from_symbol(symbol: RawSymbol) -> Type<RawSymbol> {
        match symbol {
            RawSymbol::Qualified(_, _) |
            RawSymbol::Trusted(_, _) => {
                Type::Concrete(symbol)
            }
            RawSymbol::Unqualified(name) => {
                if name.chars().nth(0).unwrap().is_uppercase() {
                    Type::Concrete(RawSymbol::Unqualified(name))
                } else {
                    Type::Var(name)
                }
            }
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Scheme<Sym> {
    pub type_: Node<Type<Sym>>,
    pub vars: Vec<(Node<String>, Node<Type<Sym>>)>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Decl<Sym> {
    Let(LetDecl<Sym>),
    Infix(Associativity, Node<String>, Node<u64>),
    TypeAlias(TypeAlias<Sym>),
    Union(UnionType<Sym>),
    Record(RecordType<Sym>),
    Trait(Trait<Sym>),
}

#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
pub enum Associativity {
    Left,
    Right,
    None,
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypeAlias<Sym> {
    pub name: Node<String>,
    pub vars: Vec<Node<String>>,
    pub type_: Node<Type<Sym>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct UnionType<Sym> {
    pub name: Node<String>,
    pub vars: Vec<Node<String>>,
    pub cases: Vec<Node<UnionCase<Sym>>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct UnionCase<Sym> {
    pub tag: Node<String>,
    pub args: Vec<Node<Type<Sym>>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct RecordType<Sym> {
    pub name: Node<String>,
    pub vars: Vec<Node<String>>,
    pub fields: Vec<(Node<String>, Node<Type<Sym>>)>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Trait<Sym> {
    pub name: Node<String>,
    pub vars: Vec<Node<String>>,
    pub values: Vec<Node<TypeAnnot<Sym>>>,
    pub base_traits: Vec<Node<Type<Sym>>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Impl<Sym> {
    pub scheme: Scheme<Sym>,
    pub trait_: Node<Type<Sym>>,
    pub values: Vec<Node<Def<Sym>>>,
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Kind {
    Star,
    Arrow(Box<Kind>, Box<Kind>),
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Hash, Clone)]
pub enum RawSymbol {
    Qualified(String, String),
    Unqualified(String),
    Trusted(String, String),
}

impl RawSymbol {
    fn full_name(self) -> String {
        match self {
            RawSymbol::Qualified(mut path, name) => {
                path.push('.');
                path.push_str(&name);
                path
            }
            RawSymbol::Trusted(mut path, name) => {
                path.push('.');
                path.push_str(&name);
                path
            }
            RawSymbol::Unqualified(name) => {
                name
            }
        }
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Hash, Clone)]
pub enum Symbol {
    Global(String, String),
    Local(String),
}
