use position::Span;
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
    Let(Vec<Node<LetDecl>>, Box<Node<Expr>>),
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
    Deconstruct(Node<Symbol>, Vec<Node<Pattern>>),
    Infix(Box<Node<Pattern>>, Node<Symbol>, Box<Node<Pattern>>),
    As(Box<Node<Pattern>>, Node<String>),
    Parenthesised(Box<Node<Pattern>>),
    Tuple(Vec<Node<Pattern>>),
    List(Vec<Node<Pattern>>),
}

impl Pattern {
    pub fn from_symbol(symbol: Symbol, span: Span) -> Pattern {
        match symbol {
            Symbol::Qualified(_, _) => {
                Pattern::Deconstruct(Node::new(symbol, span), Vec::new())
            }
            Symbol::Unqualified(name) => {
                if name.chars().nth(0).unwrap().is_uppercase() {
                    Pattern::Deconstruct(Node::new(Symbol::Unqualified(name), span), Vec::new())
                } else {
                    Pattern::Var(name)
                }
            }
        }
    }

    pub fn bound_vars(&self, my_span: Span) -> Vec<Node<&str>> {
        let mut result = Vec::new();
        self.collect_vars(&mut result, my_span);
        result
    }

    pub fn collect_vars<'a>(&'a self, result: &mut Vec<Node<&'a str>>, my_span: Span) {
        match *self {
            Pattern::As(ref pattern, ref name) => {
                result.push(Node::new(&name.value, name.span));
                pattern.value.collect_vars(result, pattern.span);
            }
            Pattern::Deconstruct(_, ref args) => {
                for arg in args {
                    arg.value.collect_vars(result, arg.span);
                }
            }
            Pattern::Infix(ref left, _, ref right) => {
                left.value.collect_vars(result, left.span);
                right.value.collect_vars(result, right.span);
            }
            Pattern::Parenthesised(ref pattern) => {
                pattern.value.collect_vars(result, pattern.span);
            }
            Pattern::Var(ref name) => {
                result.push(Node::new(name, my_span));
            }
            Pattern::Tuple(ref parts) |
            Pattern::List(ref parts) => {
                for part in parts {
                    part.value.collect_vars(result, part.span);
                }
            }
            Pattern::Wildcard |
            Pattern::Literal(_) => { }
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
    // if none, then value could not be parsed completely
    pub value: Option<Node<Expr>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypeAnnot {
    pub value: Node<String>,
    pub type_: Option<Node<Scheme>>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Type {
    SelfType,
    Var(String),
    Concrete(Symbol),
    Apply(Box<Node<Type>>, Box<Node<Type>>),
    Function(Box<Node<Type>>, Box<Node<Type>>),
    Tuple(Vec<Node<Type>>),
}

impl Type {
    pub fn from_symbol(symbol: Symbol) -> Type {
        match symbol {
            Symbol::Qualified(_, _) => {
                Type::Concrete(symbol)
            }
            Symbol::Unqualified(name) => {
                if name.chars().nth(0).unwrap().is_uppercase() {
                    Type::Concrete(Symbol::Unqualified(name))
                } else {
                    Type::Var(name)
                }
            }
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct Scheme {
    pub type_: Node<Type>,
    pub bounds: Vec<(Node<String>, Node<Symbol>)>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum Decl {
    Let(LetDecl),
    Infix(Associativity, Node<String>, Node<u64>),
    TypeAlias(TypeAlias),
    Union(UnionType),
    Record(RecordType),
    Trait(Trait),
    Impl(Impl),
}

#[derive(PartialEq, Debug, Clone)]
pub struct TypeAlias {
    pub name: Node<String>,
    pub vars: Vec<Node<String>>,
    // if none, then type could not be parsed completely
    pub type_: Option<Node<Type>>,
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
    pub base_traits: Vec<Node<Symbol>>,
    pub values: Vec<Node<TypeAnnot>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Impl {
    pub scheme: Node<Scheme>,
    pub trait_: Node<Symbol>,
    pub values: Vec<Node<Def>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ModuleDef {
    pub name: Node<String>,
    pub exposing: Node<ItemList<Node<ExposedItem>>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Import {
    pub name: Node<String>,
    pub alias: Option<Node<String>>,
    pub exposing: Option<Node<ItemList<Node<ExposedItem>>>>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct ExposedItem {
    pub name: Node<String>,
    pub subitems: Option<Node<ItemList<Node<String>>>>,
}

#[derive(PartialEq, Debug, Clone)]
pub enum ItemList<T> {
    Some(Vec<T>),
    All,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Module {
    pub def: Node<ModuleDef>,
    pub imports: Vec<Node<Import>>,
    pub items: Vec<Node<Decl>>,
}

impl Module {
    pub fn name(&self) -> &str {
        &self.def.value.name.value
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Hash, Clone)]
pub enum Symbol {
    Qualified(String, String),
    Unqualified(String),
}

impl Symbol {
    pub fn full_name(self) -> String {
        match self {
            Symbol::Qualified(mut path, name) => {
                path.push('.');
                path.push_str(&name);
                path
            }
            Symbol::Unqualified(name) => {
                name
            }
        }
    }
}
