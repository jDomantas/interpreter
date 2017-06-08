use ast::RawSymbol;


#[derive(PartialEq, Clone, Debug)]
pub enum Token {
    Ident(RawSymbol),
    Float(f64),
    Int(u64),
    Operator(String),
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

#[derive(PartialEq, Clone, Debug)]
pub enum TokenKind {
    Token(Token),
    Ident,
    UnqualifiedName,
    VarName,
    Literal,
    Operator,
    Int,
}
