use std::str::{Chars, FromStr};
use std::iter::Peekable;
use ast::Node;
use ast::parsed::Symbol;
use errors::{Error, parse_error};
use parsing::tokens::Token;
use position::{Position, Span};


pub fn lex(source: &str, module: &str) -> (Vec<Node<Token>>, Vec<Error>) {
    let mut lexer = Lexer::new(source, module);
    let mut tokens = Vec::new();
    while let Some(token) = lexer.next_token() {
        tokens.push(token);
    }
    tokens.push(lexer.make_eof_token());
    (tokens, lexer.errors)
}

struct Lexer<'a, 'b> {
    source: Peekable<Chars<'a>>,
    current: Option<char>,
    line: usize,
    column: usize,
    eof_position: Position,
    panicking: bool,
    errors: Vec<Error>,
    module: &'b str,
}

impl<'a, 'b> Lexer<'a, 'b> {
    fn new(source: &'a str, module: &'b str) -> Lexer<'a, 'b> {
        let mut chars = source.chars();
        let current = chars.next();
        Lexer {
            source: chars.peekable(),
            current: current,
            line: 1,
            column: 1,
            eof_position: Position::new(1, 1),
            panicking: false,
            errors: Vec::new(),
            module: module,
        }
    }

    fn emit_error(&mut self, message: &str, span: Span) {
        self.errors.push(parse_error(message, span, self.module));
    }

    fn peek(&self) -> Option<char> {
        self.current
    }

    fn peek2(&mut self) -> Option<char> {
        self.source.peek().cloned()
    }

    fn advance(&mut self) {
        match self.current {
            Some('\n') => {
                self.line += 1;
                self.column = 1;
            }
            Some(_) => {
                self.column += 1;
            }
            None => { }
        }
        self.current = self.source.next();
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

    fn single_char_token(&mut self, tok: Token) -> Node<Token> {
        let pos = self.current_position();
        self.advance();
        Node::new(tok, pos.span_to(pos))
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

    fn skip_block_comment(&mut self) {
        let start = self.current_position();
        assert_eq!(self.consume(), Some('{'));
        assert_eq!(self.consume(), Some('-'));
        let opened_at = start.span_to(self.current_position());
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
                    self.emit_error("unterminated block comment", opened_at);
                    break;
                }
                _ => { }
            }
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
        (ident, start.span_to(end))
    }

    fn lex_number(&mut self) -> Node<Token> {
        let (number, span) = self.collect_chars(|ch| {
           ch.is_alphanumeric() || ch == '_' || ch == '.'
        });
        debug_assert!(!number.is_empty());

        if let Ok(number) = u64::from_str(&number) {
            return Node::new(Token::Int(number), span);
        }

        if let Ok(number) = f64::from_str(&number) {
            return Node::new(Token::Float(number), span);
        }
        
        self.emit_error("invalid number literal", span);
        Node::new(Token::Error, span)
    }

    fn lex_ident(&mut self) -> Node<Token> {
        let (mut name, mut span) = self.collect_chars(is_symbol_char);
        debug_assert!(!name.is_empty());

        if let Some(token) = ident_keyword(&name) {
            return Node::new(token, span);
        }

        let mut path = String::new();

        while self.check('.') {
            match self.peek() {
                Some(ch) if is_symbol_char(ch) && !ch.is_digit(10) => {
                    let (segment, segment_span) = self.collect_chars(is_symbol_char);
                    debug_assert!(!segment.is_empty());
                    if ident_keyword(&segment).is_some() {
                        self.emit_error("path contains keyword", segment_span);
                        return Node::new(Token::Error, span);
                    }
                    if !path.is_empty() {
                        path.push('.');
                    }
                    path.push_str(&name);
                    name = segment;
                    span = span.merge(segment_span);
                }
                _ => {
                    let error_span = self.current_position().span_to(self.current_position());
                    self.emit_error("missing path segment", error_span);
                    return Node::new(Token::Error, span);
                }
            }
        }

        let symbol = if path.is_empty() {
            Symbol::Unqualified(name)
        } else {
            Symbol::Qualified(path, name)
        };

        Node::new(Token::Ident(symbol), span)
    }

    fn lex_operator(&mut self) -> Node<Token> {
        let (op, span) = self.collect_chars(is_operator_char);
        debug_assert!(!op.is_empty());

        if self.peek() == Some('}') && op.chars().next_back() == Some('-') {
            let end_position = self.current_position();
            let start_position = Position {
                line: end_position.line,
                column: end_position.column - 1,
            };
            let error_span = start_position.span_to(end_position);
            self.emit_error("unexpected end of block comment", error_span);
            self.consume();
            return Node::new(Token::Error, span);
        }

        Node::new(Token::Operator(Symbol::Unqualified(op)), span)
    }

    fn next_raw_token(&mut self) -> Option<Node<Token>> {
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
                '-' => {
                    if self.peek2() == Some('-') {
                        self.skip_line_comment();
                    } else {
                        return Some(self.lex_operator());
                    }
                }
                '{' => {
                    if self.peek2() == Some('-') {
                        self.skip_block_comment();
                    } else {
                        return Some(self.single_char_token(Token::OpenBrace));
                    }
                }
                '}' => {
                    return Some(self.single_char_token(Token::CloseBrace));
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
                    let span = self.current_position().span_to(self.current_position());
                    self.emit_error("tab character", span);
                    self.advance();
                }
                ch if ch.is_whitespace() => {
                    self.advance();
                }
                _ => {
                    let span = self.current_position().span_to(self.current_position());
                    self.emit_error("unknown character", span);
                    self.advance();
                }
            }
        }
    }

    fn next_token(&mut self) -> Option<Node<Token>> {
        while let Some(tok) = self.next_raw_token() {
            let tok = tok.map(change_special);
            self.set_eof_position(tok.span);
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

    fn set_eof_position(&mut self, after: Span) {
        let pos = Position::new(after.end.line, after.end.column + 2);
        self.eof_position = pos;
    }

    fn make_eof_token(&self) -> Node<Token> {
        let token = Token::EndOfInput;
        let span = self.eof_position.span_to(self.eof_position);
        Node::new(token, span)
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
        Token::Ident(Symbol::Unqualified(name)) => {
            match ident_keyword(&name) {
                Some(token) => token,
                None => Token::Ident(Symbol::Unqualified(name)),
            }
        }
        Token::Operator(Symbol::Unqualified(op)) => {
            match special_operator(&op) {
                Some(token) => token,
                None => Token::Operator(Symbol::Unqualified(op)),
            }
        }
        token => token,
    }
}

#[cfg(test)]
mod tests {
    use ast::Node;
    use ast::parsed::Symbol;
    use position::{Position, Span};
    use parsing::tokens::Token;
    use parsing::lexer::lex;

    fn lex_no_positions(source: &str) -> Vec<Token> {
        let (tokens, errors) = lex(source, "<test>");
        assert!(errors.is_empty());
        tokens.into_iter().map(|tok| tok.value).collect()
    }

    #[test]
    fn basic_lexing() {
        let tokens = lex_no_positions("a b 1 1.0 +");
        assert_eq!(tokens, vec![
            Token::Ident(Symbol::Unqualified("a".to_string())),
            Token::Ident(Symbol::Unqualified("b".to_string())),
            Token::Int(1),
            Token::Float(1.0),
            Token::Operator(Symbol::Unqualified("+".to_string())),
            Token::EndOfInput,
        ]);
    }

    #[test]
    fn qualified_symbol_lexing() {
        let tokens = lex_no_positions("foo bar.foo foo.baz.bar");
        assert_eq!(tokens, vec![
            Token::Ident(Symbol::Unqualified("foo".to_string())),
            Token::Ident(Symbol::Qualified("bar".to_string(), "foo".to_string())),
            Token::Ident(Symbol::Qualified("foo.baz".to_string(), "bar".to_string())),
            Token::EndOfInput,
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
            Token::EndOfInput,
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
            Token::EndOfInput,
        ]);
    }

    #[test]
    fn positions() {
        let (tokens, errors) = lex("a =\n1", "<test>");
        assert!(errors.is_empty());
        assert_eq!(tokens, vec![
            Node {
                value: Token::Ident(Symbol::Unqualified("a".to_string())),
                span: Span {
                    start: Position { line: 1, column: 1 },
                    end: Position { line: 1, column: 1 }
                },
            },
            Node {
                value: Token::Equals,
                span: Span {
                    start: Position { line: 1, column: 3 },
                    end: Position { line: 1, column: 3 }
                },
            },
            Node {
                value: Token::Int(1),
                span: Span {
                    start: Position { line: 2, column: 1 },
                    end: Position { line: 2, column: 1 }
                },
            },
            Node {
                value: Token::EndOfInput,
                span: Span {
                    start: Position { line: 2, column: 2 },
                    end: Position { line: 2, column: 2 }
                },
            }
        ]);
    }

    #[test]
    fn comments() {
        let tokens = lex_no_positions("0 {- 1 -} 2 -- 3 \n 4");
        assert_eq!(tokens, vec![
            Token::Int(0),
            Token::Int(2),
            Token::Int(4),
            Token::EndOfInput,
        ]);
    }

    #[test]
    fn ambiguous() {
        let tokens = lex_no_positions("- -- - \n { {- } -}");
        assert_eq!(tokens, vec![
            Token::Operator(Symbol::Unqualified("-".to_string())),
            Token::OpenBrace,
            Token::EndOfInput,
        ]);
    }
}
