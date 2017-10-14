use std::str::{Chars, FromStr};
use std::iter::Peekable;
use codemap::{File, Span};
use ast::parsed::Symbol;
use parsing::tokens::{WithLayout, Token};
use CompileCtx;


pub(crate) fn lex(file: &File, ctx: &mut CompileCtx) -> Vec<WithLayout<Token>> {
    let mut lexer = Lexer::new(file.source(), file.span, ctx);
    let mut tokens = Vec::new();
    while let Some(token) = lexer.next_token() {
        tokens.push(token);
    }
    tokens.push(lexer.make_eof_token());
    tokens
}

struct Lexer<'a> {
    source: Peekable<Chars<'a>>,
    current_offset: u64,
    span: Span,
    current: Option<char>,
    column: usize,
    panicking: bool,
    ctx: &'a mut CompileCtx,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str, span: Span, ctx: &'a mut CompileCtx) -> Self {
        let mut chars = source.chars();
        let current = chars.next();
        Lexer {
            source: chars.peekable(),
            current_offset: 0,
            span,
            current,
            column: 1,
            panicking: false,
            ctx,
        }
    }

    fn error(&mut self, message: &str, span: Span) {
        self.ctx.reporter
            .parse_error(message, span)
            .span_note(message, span)
            .done();
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
                self.column = 1;
                self.current_offset += 1;
            }
            Some(_) => {
                self.column += 1;
                self.current_offset += 1;
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

    fn start_span(&self) -> u64 {
        self.current_offset
    }

    fn end_span(&self, start: u64) -> Span {
        self.span.subspan(start, self.current_offset as u64)
    }

    fn single_char_token(&mut self, tok: Token) -> WithLayout<Token> {
        let start = self.start_span();
        let col = self.column;
        self.advance();
        let span = self.end_span(start);
        WithLayout {
            value: tok,
            span,
            col,
        }
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
        let start = self.start_span();
        assert_eq!(self.consume(), Some('{'));
        assert_eq!(self.consume(), Some('-'));
        let opened_at = self.end_span(start);
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
                    self.error("Unterminated block comment.", opened_at);
                    break;
                }
                _ => { }
            }
        }
    }

    fn collect_chars<F>(&mut self, mut can_take: F) -> WithLayout<String>
        where F: FnMut(char) -> bool
    {
        let start = self.start_span();
        let mut span = self.end_span(start);
        let col = self.column;
        let mut ident = String::new();
        while let Some(ch) = self.peek() {
            if can_take(ch) {
                ident.push(ch);
                self.advance();
                span = self.end_span(start);
            } else {
                span = self.end_span(start);
                break;
            }
        }

        assert!(!ident.is_empty());
        WithLayout {
            value: ident,
            span,
            col,
        }
    }

    fn lex_number(&mut self) -> WithLayout<Token> {
        let WithLayout { value: number, span, col } = self.collect_chars(|ch| {
            is_symbol_char(ch) || ch == '.'
        });
        debug_assert!(!number.is_empty());

        let tok = if let Ok(number) = u64::from_str(&number) {
            Token::Int(number)
        } else if let Ok(number) = f64::from_str(&number) {
            Token::Float(number)
        } else {
            self.error("Invalid number literal.", span);
            Token::Error
        };

        WithLayout {
            value: tok,
            span,
            col,
        }
    }

    fn lex_ident(&mut self) -> WithLayout<Token> {
        let WithLayout {
            value: mut name,
            mut span,
            col,
        } = self.collect_chars(is_symbol_char);
        debug_assert!(!name.is_empty());

        if let Some(token) = ident_keyword(&name) {
            return WithLayout { value: token, span, col };
        }

        let mut path = String::new();

        while self.check('.') {
            match self.peek() {
                Some(ch) if is_symbol_char(ch) && !ch.is_digit(10) => {
                    let WithLayout {
                        value: segment,
                        span: segment_span,
                        ..
                    } = self.collect_chars(is_symbol_char);
                    debug_assert!(!segment.is_empty());
                    if ident_keyword(&segment).is_some() {
                        self.error("Path segment is a keyword.", segment_span);
                        return WithLayout {
                            value: Token::Error,
                            span,
                            col,
                        };
                    }
                    if !path.is_empty() {
                        path.push('.');
                    }
                    path.push_str(&name);
                    name = segment;
                    span = span.merge(segment_span);
                }
                _ => {
                    let error_span = self.span.subspan(
                        self.current_offset,
                        self.current_offset,
                    );
                    self.error("Expected path segment after dot.", error_span);
                    return WithLayout {
                        value: Token::Error,
                        span,
                        col,
                    };
                }
            }
        }

        let symbol = if path.is_empty() {
            Symbol::Unqualified(name)
        } else {
            Symbol::Qualified(path, name)
        };

        WithLayout {
            value: Token::Ident(symbol),
            span,
            col,
        }
    }

    fn lex_operator(&mut self) -> WithLayout<Token> {
        let WithLayout {
            value: op,
            span,
            col,
        } = self.collect_chars(is_operator_char);
        debug_assert!(!op.is_empty());

        let tok = if self.peek() == Some('}') && op.chars().next_back() == Some('-') {
            let span = self.span.subspan(
                self.current_offset - 1,
                self.current_offset + 1,
            );
            self.error("Unexpected end of block comment.", span);
            self.consume();
            Token::Error
        } else {
            Token::Operator(Symbol::Unqualified(op))
        };

        WithLayout {
            value: tok,
            span,
            col,
        }
    }

    fn lex_string_literal(&mut self) -> WithLayout<Token> {
        let start = self.start_span();
        let col = self.column;
        assert_eq!(self.consume(), Some('"'));
        let span = self.end_span(start);
        if self.check('"') {
            return WithLayout {
                value: Token::Str(String::new()),
                span,
                col,
            };
        }
        let string = self.collect_chars(|c| c != '"').value;
        let span = self.end_span(start);
        let tok = match self.consume() {
            Some('"') => {
                Token::Str(string)
            }
            None => {
                self.error("Unterminated string literal", span);
                Token::Error
            }
            _ => {
                unreachable!()
            }
        };

        WithLayout {
            value: tok,
            span,
            col,
        }
    }

    fn lex_char_literal(&mut self) -> WithLayout<Token> {
        let start = self.start_span();
        let col = self.column;
        assert_eq!(self.consume(), Some('\''));
        let span = self.end_span(start);
        let ch = match self.consume() {
            Some(c) if c != '\'' => c,
            _ => {
                self.error("Invalid char literal", span);
                return WithLayout {
                    value: Token::Error,
                    span,
                    col,
                };
            }
        };
        let span = self.end_span(start);
        let tok = match self.consume() {
            Some('\'') => {
                Token::Char(ch)
            }
            _ => {
                self.error("Invalid char literal", span);
                Token::Error
            }
        };
        WithLayout {
            value: tok,
            span,
            col,
        }
    }

    fn next_raw_token(&mut self) -> Option<WithLayout<Token>> {
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
                ';' => {
                    return Some(self.single_char_token(Token::Semicolon));
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
                    let span = self.span.subspan(
                        self.current_offset,
                        self.current_offset + 1,
                    );
                    self.error("Found tab character. Please indent your code using spaces.", span);
                    self.advance();
                }
                ch if ch.is_whitespace() => {
                    self.advance();
                }
                '"' => {
                    return Some(self.lex_string_literal());
                }
                '\'' => {
                    return Some(self.lex_char_literal());
                }
                _ => {
                    let span = self.span.subspan(
                        self.current_offset,
                        self.current_offset + 1,
                    );
                    self.error("Found unknown character.", span);
                    self.advance();
                }
            }
        }
    }

    fn next_token(&mut self) -> Option<WithLayout<Token>> {
        while let Some(tok) = self.next_raw_token() {
            let tok = tok.map(change_special);
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

    fn make_eof_token(&self) -> WithLayout<Token> {
        let token = Token::EndOfInput;
        let span = self.span.subspan(self.span.len(), self.span.len());
        WithLayout {
            value: token,
            span,
            col: 0,
        }
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
        "forall" => Some(Token::Forall),
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
        "." => Some(Token::Dot),
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
    use ast::parsed::Symbol;
    use parsing::tokens::Token;
    use parsing::lexer::lex;
    use CompileCtx;

    fn lex_no_positions(source: &str) -> Vec<Token> {
        let mut ctx = CompileCtx::new();
        let file = ctx.codemap.add_file("Main".into(), source.into());
        let tokens = lex(&file, &mut ctx);
        assert!(!ctx.reporter.have_errors());
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
        let tokens = lex_no_positions(": <- | , ..");
        assert_eq!(tokens, vec![
            Token::Colon,
            Token::BackArrow,
            Token::Pipe,
            Token::Comma,
            Token::DotDot,
            Token::EndOfInput,
        ]);
    }

    #[test]
    fn positions() {
        let mut ctx = CompileCtx::new();
        let source = "a = \n1";
        let file = ctx.codemap.add_file("<test>".into(), source.into());
        let tokens = lex(&file, &mut ctx);
        assert!(!ctx.reporter.have_errors());
        assert_eq!(tokens.len(), 4);
        assert_eq!(tokens[0].value, Token::Ident(Symbol::Unqualified("a".to_string())));
        assert_eq!(tokens[1].value, Token::Equals);
        assert_eq!(tokens[2].value, Token::Int(1));
        assert_eq!(tokens[3].value, Token::EndOfInput);
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

    #[test]
    fn char_literal() {
        let tokens = lex_no_positions("  'a' 'b' ");
        assert_eq!(tokens, vec![
            Token::Char('a'),
            Token::Char('b'),
            Token::EndOfInput,
        ]);
    }
    
    #[test]
    fn string_literal() {
        let tokens = lex_no_positions(r#" "abc" "012" "" "#);
        assert_eq!(tokens, vec![
            Token::Str("abc".into()),
            Token::Str("012".into()),
            Token::Str("".into()),
            Token::EndOfInput,
        ]);
    }
}
