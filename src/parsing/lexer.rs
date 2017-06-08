use std::str::{Chars, FromStr};
use std::iter::Peekable;
use ast::RawSymbol;
use parsing::tokens::Token;
use position::{Position, Span, Spanned};


pub fn lex(source: &str) -> Vec<Spanned<Token>> {
    let mut lexer = Lexer::new(source);
    let mut tokens = Vec::new();
    while let Some(token) = lexer.next_token() {
        tokens.push(token);
    }
    tokens
}

struct Lexer<'a> {
    source: Peekable<Chars<'a>>,
    line: usize,
    column: usize,
    panicking: bool,
}

impl<'a> Lexer<'a> {
    fn new(source: &str) -> Lexer {
        Lexer {
            source: source.chars().peekable(),
            line: 1,
            column: 1,
            panicking: false,
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.source.peek().map(|x| *x)
    }

    fn advance(&mut self) {
        match self.source.next() {
            Some('\n') => {
                self.line += 1;
                self.column = 1;
            }
            Some(_) => {
                self.column += 1;
            }
            None => { }
        }
    }

    fn consume(&mut self) -> Option<char> {
        let result = self.peek();
        self.advance();
        result
    }

    fn check(&mut self, ch: char) -> bool {
        if self.peek() == Some(ch) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn current_position(&self) -> Position {
        Position::new(self.line, self.column)
    }

    fn single_char_token(&mut self, tok: Token) -> Spanned<Token> {
        let start = self.current_position();
        self.advance();
        let end = self.current_position();
        Spanned::new(tok, start.span_to(&end))
    }

    fn skip_line_comment(&mut self) {
        loop {
            match self.consume() {
                Some('\n') | None => {
                    break;
                }
                _ => { }
            }
        }
    }

    fn skip_block_comment(&mut self, opened_at: Span) {
        let mut nesting = 1_usize;
        loop {
            match self.consume() {
                Some('{') => {
                    if self.check('-') {
                        nesting += 1;
                    }
                }
                Some('-') => {
                    if self.check('}') {
                        nesting -= 1;
                        if nesting == 0 {
                            return;
                        }
                    }
                }
                None => {
                    // TODO: unterminated block comment, report error?
                    panic!("unterminated block comment: {:?}", opened_at);
                }
                _ => { }
            }
        }
    }

    fn check_block_comment(&mut self) {
        let position = self.current_position();
        self.advance();
        if self.check('-') {
            let opened_at = position.span_to(&self.current_position());
            self.skip_block_comment(opened_at);
        } else {
            let error_span = position.span_to(&position);
            // TODO: invalid '{', report error?
            panic!("unknown char at {:?}", error_span);
        }
    }

    fn collect_chars<F: FnMut(char) -> bool>(&mut self, mut can_take: F) -> (String, Span) {
        let start = self.current_position();
        let mut end = self.current_position();
        let mut ident = String::new();
        while let Some(ch) = self.peek() {
            if can_take(ch) {
                end = self.current_position();
                ident.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        assert!(!ident.is_empty());
        return (ident, start.span_to(&end));
    }

    fn lex_number(&mut self) -> Spanned<Token> {
        let (number, span) = self.collect_chars(|ch| {
           ch.is_alphanumeric() || ch == '_' || ch == '.'
        });

        if let Ok(number) = u64::from_str(&number) {
            return Spanned::new(Token::Int(number), span);
        }

        if let Ok(number) = f64::from_str(&number) {
            return Spanned::new(Token::Float(number), span);
        }
        
        // TODO: error reporting
        panic!("bad number ({}) at {:?}", number, span);
    }

    fn lex_ident(&mut self) -> Spanned<Token> {
        // TODO: lex idents with dots in names
        let (mut name, mut span) = self.collect_chars(is_symbol_char);

        if let Some(token) = ident_keyword(&name) {
            return Spanned::new(token, span);
        }

        let mut path = String::new();

        while self.check('.') {
            match self.peek() {
                Some(ch) if is_symbol_char(ch) && !ch.is_digit(10) => {
                    let (segment, segment_span) = self.collect_chars(is_symbol_char);
                    if ident_keyword(&segment).is_some() {
                        // TODO: report error, keyword in path
                        panic!("keyword in path");
                    }
                    if !path.is_empty() {
                        path.push('.');
                    }
                    path.push_str(&name);
                    name = segment;
                    span = span.merge(&segment_span);
                }
                _ => {
                    // TODO: report error, expected path to continue
                    panic!("expected identifier");
                }
            }
        }

        let symbol = if path.is_empty() {
            RawSymbol::Unqualified(name)
        } else {
            RawSymbol::Qualified(path, name)
        };

        Spanned::new(Token::Ident(symbol), span)
    }

    fn lex_operator(&mut self) -> Spanned<Token> {
        let (op, span) = self.collect_chars(is_operator_char);

        Spanned::new(Token::Operator(op), span)
    }

    fn next_raw_token(&mut self) -> Option<Spanned<Token>> {
        loop {
            let next_char = match self.peek() {
                Some(ch) => ch,
                None => return None,
            };
            match next_char {
                '(' => {
                    return Some(self.single_char_token(Token::OpenParen));
                }
                ')' => {
                    return Some(self.single_char_token(Token::CloseParen));
                }
                '[' => {
                    return Some(self.single_char_token(Token::OpenBracket));
                }
                ']' => {
                    return Some(self.single_char_token(Token::CloseBracket));
                }
                ',' => {
                    return Some(self.single_char_token(Token::Comma));
                }
                '\\' => {
                    return Some(self.single_char_token(Token::Backslash));
                }
                ch if ch.is_digit(10) => {
                    return Some(self.lex_number());
                }
                ch if ch.is_alphabetic() || ch == '_' => {
                    return Some(self.lex_ident());
                }
                ch if is_operator_char(ch) => {
                    return Some(self.lex_operator());
                }
                '\t' => {
                    // TODO: report error, tab character
                    panic!("tab found at {:?}", self.current_position());
                }
                ch if ch.is_whitespace() => {
                    self.advance();
                }
                _ => {
                    // TODO: report error, unrecognized char
                    panic!("unknown char at {:?}", self.current_position());
                }
            }
        }
    }

    fn next_token(&mut self) -> Option<Spanned<Token>> {
        while let Some(tok) = self.next_raw_token() {
            let tok = Spanned::new(change_special(tok.value), tok.span);
            if !tok.value.is_error() {
                self.panicking = false;
                return Some(tok);
            } else if !self.panicking {
                self.panicking = true;
                return Some(tok);
            }
        }
        None
    }
}

fn is_symbol_char(ch: char) -> bool {
    ch.is_alphanumeric() || ch == '_'
}

fn is_operator_char(ch: char) -> bool {
    "~!@#$%^&*-=+|:/?<>.".contains(ch)
}

fn ident_keyword(ident: &str) -> Option<Token> {
    match ident {
        "module" => Some(Token::Module),
        "exposing" => Some(Token::Exposing),
        "import" => Some(Token::Import),
        "as" => Some(Token::As),
        "infix" => Some(Token::Infix),
        "infixl" => Some(Token::Infixl),
        "infixr" => Some(Token::Infixr),
        "type" => Some(Token::Type),
        "alias" => Some(Token::Alias),
        "trait" => Some(Token::Trait),
        "where" => Some(Token::Where),
        "self" => Some(Token::Self_),
        "impl" => Some(Token::Impl),
        "let" => Some(Token::Let),
        "in" => Some(Token::In),
        "case" => Some(Token::Case),
        "of" => Some(Token::Of),
        "if" => Some(Token::If),
        "then" => Some(Token::Then),
        "else" => Some(Token::Else),
        "do" => Some(Token::Do),
        "true" => Some(Token::Bool(true)),
        "false" => Some(Token::Bool(false)),
        "_" => Some(Token::Underscore),
        _ => None,
    }
}

fn special_operator(op: &str) -> Option<Token> {
    match op {
        ":" => Some(Token::Colon),
        "->" => Some(Token::Arrow),
        "=>" => Some(Token::FatArrow),
        "=" => Some(Token::Equals),
        "\\" => Some(Token::Backslash),
        ".." => Some(Token::DotDot),
        "," => Some(Token::Comma),
        "|" => Some(Token::Pipe),
        "<-" => Some(Token::BackArrow),
        _ => None,
    }
}

fn change_special(token: Token) -> Token {
    match token {
        Token::Ident(RawSymbol::Unqualified(name)) => {
            match ident_keyword(&name) {
                Some(token) => token,
                None => Token::Ident(RawSymbol::Unqualified(name)),
            }
        }
        Token::Operator(op) => {
            match special_operator(&op) {
                Some(token) => token,
                None => Token::Operator(op),
            }
        }
        token => token,
    }
}

#[cfg(test)]
mod tests {
    use ast::RawSymbol;
    use position::{Position, Span, Spanned};
    use parsing::tokens::Token;
    use parsing::lexer::lex;

    fn lex_no_positions(source: &str) -> Vec<Token> {
        lex(source).into_iter().map(|tok| tok.value).collect()
    }

    #[test]
    fn basic_lexing() {
        let tokens = lex_no_positions("a b 1 1.0 +");
        assert_eq!(tokens, vec![
            Token::Ident(RawSymbol::Unqualified("a".to_string())),
            Token::Ident(RawSymbol::Unqualified("b".to_string())),
            Token::Int(1),
            Token::Float(1.0),
            Token::Operator("+".to_string()),
        ]);
    }

    #[test]
    fn qualified_symbol_lexing() {
        let tokens = lex_no_positions("foo bar.foo foo.baz.bar");
        assert_eq!(tokens, vec![
            Token::Ident(RawSymbol::Unqualified("foo".to_string())),
            Token::Ident(RawSymbol::Qualified("bar".to_string(), "foo".to_string())),
            Token::Ident(RawSymbol::Qualified("foo.baz".to_string(), "bar".to_string())),
        ]);
    }

    #[test]
    fn keywords() {
        let tokens = lex_no_positions("import let if true impl");
        assert_eq!(tokens, vec![
            Token::Import,
            Token::Let,
            Token::If,
            Token::Bool(true),
            Token::Impl,
        ]);
    }

    #[test]
    fn special_operators() {
        let tokens = lex_no_positions(": => | , ..");
        assert_eq!(tokens, vec![
            Token::Colon,
            Token::FatArrow,
            Token::Pipe,
            Token::Comma,
            Token::DotDot,
        ]);
    }

    #[test]
    fn positions() {
        let tokens = lex("a =\n1");
        assert_eq!(tokens, vec![
            Spanned {
                value: Token::Ident(RawSymbol::Unqualified("a".to_string())),
                span: Span {
                    start: Position { line: 1, column: 1 },
                    end: Position { line: 1, column: 1 }
                },
            },
            Spanned {
                value: Token::Equals,
                span: Span {
                    start: Position { line: 1, column: 3 },
                    end: Position { line: 1, column: 3 }
                },
            },
            Spanned {
                value: Token::Int(1),
                span: Span {
                    start: Position { line: 2, column: 1 },
                    end: Position { line: 2, column: 1 }
                },
            },
        ]);
    }

    #[test]
    fn comments() {
        let tokens = lex_no_positions("0 {- 1 -} 2 -- 3 \n 4");
        assert_eq!(tokens, vec![
            Token::Int(0),
            Token::Int(2),
            Token::Int(4),
        ]);
    }

    #[test]
    fn ambiguous() {
        let tokens = lex_no_positions("- -- - \n { {- } -}");
        assert_eq!(tokens, vec![
            Token::Operator("-".to_string()),
            Token::OpenBrace,
        ]);
    }
}
