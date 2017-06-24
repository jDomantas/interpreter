use ast::parsed::Symbol;


#[derive(PartialEq, PartialOrd, Clone, Debug)]
pub enum Token {
    Ident(Symbol),
    Float(f64),
    Int(u64),
    Operator(Symbol),
    Bool(bool),
    Module,
    Exposing,
    Import,
    As,
    Infix,
    Infixl,
    Infixr,
    Type,
    Alias,
    Trait,
    Where,
    Self_,
    Impl,
    Let,
    In,
    Case,
    Of,
    If,
    Then,
    Else,
    Do,
    Underscore,
    Colon,
    Arrow,
    FatArrow,
    BackArrow,
    Equals,
    Backslash,
    DotDot,
    Comma,
    Pipe,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenBrace,
    CloseBrace,
    EndOfInput,
    Error,
}

impl Token {
    pub fn is_error(&self) -> bool {
        match *self {
            Token::Error => true,
            _ => false,
        }
    }
}

#[derive(PartialEq, PartialOrd, Clone, Debug)]
pub enum TokenKind {
    Token(Token),
    Ident,
    UnqualifiedName,
    VarName,
    Literal,
    Operator,
    UnqualifiedOperator,
    Int,
}

impl TokenKind {
    pub fn to_string(&self) -> &'static str {
        match *self {
            TokenKind::Ident => "identifier",
            TokenKind::UnqualifiedName => "unqualified name",
            TokenKind::VarName => "var name",
            TokenKind::Literal => "literal",
            TokenKind::Operator => "operator",
            TokenKind::UnqualifiedOperator => "unqualified operator",
            TokenKind::Int => "integer value",
            TokenKind::Token(Token::Alias) => "'alias'",
            TokenKind::Token(Token::Arrow) => "'->'",
            TokenKind::Token(Token::As) => "'as'",
            TokenKind::Token(Token::BackArrow) => "'<-'",
            TokenKind::Token(Token::Backslash) => "'\\'",
            TokenKind::Token(Token::Case) => "'case'",
            TokenKind::Token(Token::CloseBrace) => "'}'",
            TokenKind::Token(Token::CloseBracket) => "']'",
            TokenKind::Token(Token::CloseParen) => "')'",
            TokenKind::Token(Token::Colon) => "':'",
            TokenKind::Token(Token::Comma) => "','",
            TokenKind::Token(Token::Do) => "'do'",
            TokenKind::Token(Token::DotDot) => "'..'",
            TokenKind::Token(Token::Else) => "'else'",
            TokenKind::Token(Token::Equals) => "'='",
            TokenKind::Token(Token::Exposing) => "'exposing'",
            TokenKind::Token(Token::FatArrow) => "'=>'",
            TokenKind::Token(Token::If) => "'if'",
            TokenKind::Token(Token::Impl) => "'impl'",
            TokenKind::Token(Token::Import) => "'import'",
            TokenKind::Token(Token::In) => "'in'",
            TokenKind::Token(Token::Infix) => "'infix'",
            TokenKind::Token(Token::Infixl) => "'infixl'",
            TokenKind::Token(Token::Infixr) => "'infixr'",
            TokenKind::Token(Token::Let) => "'let'",
            TokenKind::Token(Token::Module) => "'module'",
            TokenKind::Token(Token::Of) => "'of'",
            TokenKind::Token(Token::OpenBrace) => "'{'",
            TokenKind::Token(Token::OpenBracket) => "'['",
            TokenKind::Token(Token::OpenParen) => "'('",
            TokenKind::Token(Token::Pipe) => "'|'",
            TokenKind::Token(Token::Self_) => "'self'",
            TokenKind::Token(Token::Then) => "'then'",
            TokenKind::Token(Token::Trait) => "'trait'",
            TokenKind::Token(Token::Type) => "'type'",
            TokenKind::Token(Token::Underscore) => "'_'",
            TokenKind::Token(Token::Where) => "'where'",
            TokenKind::Token(Token::Ident(_)) |
            TokenKind::Token(Token::Operator(_)) |
            TokenKind::Token(Token::Int(_)) |
            TokenKind::Token(Token::Float(_)) |
            TokenKind::Token(Token::Bool(_)) |
            TokenKind::Token(Token::EndOfInput) |
            TokenKind::Token(Token::Error) => {
                panic!("{:?} is not supposed to be constructed", self);  
            },
        }
    }

    pub fn matches_token(&self, token: &Token) -> bool {
        match *token {
            Token::Ident(ref ident) => {
                match *self {
                    TokenKind::Ident => true,
                    TokenKind::UnqualifiedName => {
                        match *ident {
                            Symbol::Qualified(_, _) => false,
                            Symbol::Unqualified(_) => true,
                        }
                    }
                    TokenKind::VarName => {
                        match *ident {
                            Symbol::Qualified(_, _) => false,
                            Symbol::Unqualified(ref name) =>
                                !name.chars().nth(0).unwrap().is_uppercase(),
                        }
                    }
                    _ => false,
                }
            }
            Token::Float(_) | Token::Bool(_) => {
                self == &TokenKind::Literal
            }
            Token::Operator(Symbol::Unqualified(_)) => {
                self == &TokenKind::Operator || self == &TokenKind::UnqualifiedOperator
            }
            Token::Operator(Symbol::Qualified(_, _)) => {
                self == &TokenKind::Operator
            }
            Token::Int(_) => {
                self == &TokenKind::Literal || self == &TokenKind::Int
            }
            ref token => {
                match *self {
                    TokenKind::Token(ref tok) => tok == token,
                    _ => false,
                }
            }
        }
    }
}
