use std::collections::HashMap;
use ast::{Node, Name, Literal, Associativity};


#[derive(Debug, Clone)]
pub enum Expr {
    Ident(Symbol),
    Literal(Literal),
    Apply(Box<Node<Expr>>, Box<Node<Expr>>),
    If(Box<Node<Expr>>, Box<Node<Expr>>, Box<Node<Expr>>),
    Infix(Box<Node<Expr>>, Node<Symbol>, Box<Node<Expr>>),
    Parenthesised(Box<Node<Expr>>),
    Lambda(Node<Sym>, Box<Node<Expr>>),
    Case(Box<Node<Expr>>, Vec<Node<CaseBranch>>),
    Let(Vec<Node<Def>>, Box<Node<Expr>>),
    List(Vec<Node<Expr>>),
    Tuple(Vec<Node<Expr>>),
    Bind(Node<Pattern>, Box<Node<Expr>>, Box<Node<Expr>>),
    DoIf(Box<Node<Expr>>, Box<Node<Expr>>),
}

#[derive(Debug, Clone)]
pub struct CaseBranch {
    pub pattern: Node<Pattern>,
    pub value: Node<Expr>,
    pub guard: Option<Node<Expr>>,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Literal(Literal),
    Deconstruct(Node<Symbol>, Vec<Node<Pattern>>),
    Infix(Box<Node<Pattern>>, Node<Symbol>, Box<Node<Pattern>>),
    As(Box<Node<Pattern>>, Node<Sym>),
    Parenthesised(Box<Node<Pattern>>),
    Tuple(Vec<Node<Pattern>>),
    List(Vec<Node<Pattern>>),
}

impl Pattern {
    pub fn bound_vars(&self) -> Vec<Sym> {
        let mut vars = Vec::new();
        self.collect_vars(&mut vars);
        vars
    }

    fn collect_vars(&self, vars: &mut Vec<Sym>) {
        match *self {
            Pattern::Wildcard |
            Pattern::Literal(_) => { }
            Pattern::Infix(ref lhs, _, ref rhs) => {
                lhs.value.collect_vars(vars);
                rhs.value.collect_vars(vars);
            }
            Pattern::As(ref pat, ref sym) => {
                vars.push(sym.value);
                pat.value.collect_vars(vars);
            }
            Pattern::Parenthesised(ref pat) => {
                pat.value.collect_vars(vars);
            }
            Pattern::List(ref items) |
            Pattern::Tuple(ref items) |
            Pattern::Deconstruct(_, ref items) => {
                for item in items {
                    item.value.collect_vars(vars);
                }
            }
        }
    }

    pub fn only_with_var(&self, var: Sym, to: Sym) -> Pattern {
        match *self {
            Pattern::Wildcard => Pattern::Wildcard,
            Pattern::Literal(ref lit) => Pattern::Literal(lit.clone()),
            Pattern::Deconstruct(ref sym, ref parts) => {
                let parts = parts.iter().map(|p| {
                    Node::new(p.value.only_with_var(var, to), p.span)
                });
                Pattern::Deconstruct(sym.clone(), parts.collect())
            }
            Pattern::Infix(ref lhs, ref sym, ref rhs) => {
                let lhs = Node::new(lhs.value.only_with_var(var, to), lhs.span);
                let rhs = Node::new(rhs.value.only_with_var(var, to), rhs.span);
                let sym = sym.clone();
                Pattern::Infix(Box::new(lhs), sym, Box::new(rhs))
            }
            Pattern::As(ref pat, ref alias) => {
                let pat = Node::new(pat.value.only_with_var(var, to), pat.span);
                if alias.value == var {
                    let sym = Node::new(to, alias.span);
                    Pattern::As(Box::new(pat), sym)
                } else {
                    pat.value
                }
            }
            Pattern::Parenthesised(ref pat) => {
                let pat = Node::new(pat.value.only_with_var(var, to), pat.span);
                Pattern::Parenthesised(Box::new(pat))
            }
            Pattern::List(ref items) => {
                let items = items.iter().map(|p| {
                    Node::new(p.value.only_with_var(var, to), p.span)
                });
                Pattern::List(items.collect())
            }
            Pattern::Tuple(ref items) => {
                let items = items.iter().map(|p| {
                    Node::new(p.value.only_with_var(var, to), p.span)
                });
                Pattern::Tuple(items.collect())
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Def {
    pub sym: Node<Sym>,
    pub value: Node<Expr>,
    pub module: Name,
    pub artificial: bool,
}

#[derive(Debug, Clone)]
pub struct TypeAnnot {
    pub value: Node<Sym>,
    pub type_: Node<Scheme>,
    pub module: Name,
}

#[derive(Debug, Clone)]
pub enum Type {
    Any,
    SelfType,
    Var(Sym),
    Concrete(Symbol),
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

    pub fn contains_var(&self, var: Sym) -> bool {
        match *self {
            Type::Any => false,
            Type::Var(v) => var == v,
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

#[derive(Debug, Clone)]
pub struct Scheme {
    pub type_: Node<Type>,
    pub bounds: Vec<(Node<Sym>, Node<Symbol>)>,
}

#[derive(Debug, Clone)]
pub enum TypeDecl {
    TypeAlias(TypeAlias),
    Union(UnionType),
    Record(RecordType),
}

impl TypeDecl {
    pub fn name(&self) -> Sym {
        match *self {
            TypeDecl::Record(ref record) => record.name.value,
            TypeDecl::TypeAlias(ref alias) => alias.name.value,
            TypeDecl::Union(ref union) => union.name.value,
        }
    }

    pub fn var_list(&self) -> &[Node<Sym>] {
        match *self {
            TypeDecl::Record(ref record) => &record.vars,
            TypeDecl::TypeAlias(ref alias) => &alias.vars,
            TypeDecl::Union(ref union) => &union.vars,
        }
    }

    pub fn module(&self) -> &Name {
        match *self {
            TypeDecl::Record(ref record) => &record.module,
            TypeDecl::TypeAlias(ref alias) => &alias.module,
            TypeDecl::Union(ref union) => &union.module,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeAlias {
    pub name: Node<Sym>,
    pub vars: Vec<Node<Sym>>,
    pub type_: Option<Node<Type>>,
    pub module: Name,
}

#[derive(Debug, Clone)]
pub struct UnionType {
    pub name: Node<Sym>,
    pub vars: Vec<Node<Sym>>,
    pub cases: Vec<Node<UnionCase>>,
    pub module: Name,
}

#[derive(Debug, Clone)]
pub struct UnionCase {
    pub tag: Node<Sym>,
    pub args: Vec<Node<Type>>,
}

#[derive(Debug, Clone)]
pub struct RecordType {
    pub name: Node<Sym>,
    pub vars: Vec<Node<Sym>>,
    pub fields: Vec<(Node<Sym>, Node<Type>)>,
    pub module: Name,
}

#[derive(Debug, Clone)]
pub struct Trait {
    pub name: Node<Sym>,
    pub base_traits: Vec<Node<Symbol>>,
    pub values: Vec<Node<TypeAnnot>>,
    pub module: Name,
}

#[derive(Debug, Clone)]
pub struct Impl {
    pub scheme: Node<Scheme>,
    pub trait_: Node<Symbol>,
    pub values: Vec<Node<Def>>,
    // mapping from impl symbols to trait symbols
    pub trait_items: HashMap<Sym, Sym>,
    pub module: Name,
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Hash, Copy, Clone)]
pub struct Sym(u64);

impl Sym {
    pub fn new(id: u64) -> Sym {
        Sym(id)
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Hash, Copy, Clone)]
pub enum Symbol {
    Known(Sym),
    Unknown,
}

#[derive(Default)]
pub struct Items {
    pub types: Vec<TypeDecl>,
    pub items: Vec<Def>,
    pub traits: Vec<Trait>,
    pub impls: Vec<Impl>,
    pub annotations: HashMap<Sym, Node<TypeAnnot>>,
    pub fixities: HashMap<Sym, (Associativity, u64)>,
    pub symbol_names: HashMap<Sym, String>,
}

impl Items {
    pub fn new() -> Items {
        Default::default()
    }
}
