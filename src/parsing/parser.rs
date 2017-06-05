use std::cmp::Ordering;
use std::iter::Peekable;
use position::{Span, Spanned, DUMMY_SPAN};
use parsing::tokens::Token;
use ast::{Expr, Literal, Pattern, Node};


pub fn parse<I: Iterator<Item=Spanned<Token>>>(_tokens: I) -> ! {
    // let mut parser = Parser::new(tokens);
    unimplemented!()
}

#[derive(PartialEq, Debug, Clone)]
enum TokenKind {
    Token(Token),
    Ident,
    Literal,
    Operator,
}

fn error_expr_node() -> Node<Expr> {
    Node::new(Expr::Error, DUMMY_SPAN.clone())
}

fn error_pattern_node() -> Node<Pattern> {
    Node::new(Pattern::Error, DUMMY_SPAN.clone())
}

type ParseResult<T> = Result<T, T>;

enum MaybeParsed<T> {
    Ok(T),
    Err(T),
    Empty,
}

struct ParseError;

struct Parser<I: Iterator<Item=Spanned<Token>>> {
    tokens: Peekable<I>,
    expected_tokens: Vec<TokenKind>,
    errors: Vec<ParseError>,
    current_indent: usize,
    accept_aligned: bool,
    last_token_span: Span,
}

impl<I: Iterator<Item=Spanned<Token>>> Parser<I> {
    fn new(tokens: I) -> Parser<I> {
        Parser {
            tokens: tokens.peekable(),
            expected_tokens: Vec::new(),
            errors: Vec::new(),
            current_indent: 0,
            accept_aligned: false,
            last_token_span: DUMMY_SPAN.clone(),
        }
    }

    fn emit_error(&mut self) {
        unimplemented!();
    }

    fn previous_span(&self) -> Span {
        self.last_token_span.clone()
    }

    fn peek(&mut self) -> Option<&Spanned<Token>> {
        match self.tokens.peek() {
            Some(ref token) => {
                let token_column = token.span.start.column;
                let order = if self.accept_aligned {
                    Ordering::Equal
                } else {
                    Ordering::Greater
                };
                if token_column.cmp(&self.current_indent) == order {
                    Some(token)
                } else {
                    None
                }
            }
            None => {
                None
            }
        }
    }

    fn consume(&mut self) -> Option<Spanned<Token>> {
        self.last_token_span = match self.peek() {
            Some(ref tok) => tok.span.clone(),
            None => DUMMY_SPAN.clone(),
        };
        self.accept_aligned = false;
        self.expected_tokens.clear();
        self.tokens.next()
    }

    fn eat(&mut self, expected: Token) -> bool {
        let is_good = match self.peek() {
            Some(token) if token.value == expected => true,
            _ => false,
        };

        if is_good {
            self.consume();
        } else {
            self.expected_tokens.push(TokenKind::Token(expected));
        }

        is_good
    }

    fn eat_symbol(&mut self) -> Option<Spanned<String>> {
        match self.peek() {
            Some(&Spanned { value: Token::Ident(_), .. }) => { }
            _ => {
                self.expected_tokens.push(TokenKind::Ident);
                return None;
            }
        }

        match self.consume() {
            Some(Spanned { value: Token::Ident(ident), span }) => {
                Some(Spanned::new(ident, span))
            }
            _ => {
                panic!("Parser::peek and Parser::consume returned different results");
            }
        }
    }
    
    fn eat_literal(&mut self) -> Option<Spanned<Literal>> {
        match self.peek() {
            Some(&Spanned { value: Token::Int(_), .. }) |
            Some(&Spanned { value: Token::Float(_), .. }) |
            Some(&Spanned { value: Token::Bool(_), .. }) => { }
            _ => {
                self.expected_tokens.push(TokenKind::Literal);
                return None;
            }
        }

        match self.consume() {
            Some(Spanned { value: Token::Int(int), span }) => {
                Some(Spanned::new(Literal::Int(int), span))
            }
            Some(Spanned { value: Token::Float(float), span }) => {
                Some(Spanned::new(Literal::Float(float), span))
            }
            Some(Spanned { value: Token::Bool(b), span }) => {
                Some(Spanned::new(Literal::Bool(b), span))
            }
            _ => {
                panic!("Parser::peek and Parser::consume returned different results");
            }
        }
    }

    fn eat_operator(&mut self) -> Option<Spanned<String>> {
        match self.peek() {
            Some(&Spanned { value: Token::Operator(_), .. }) => { }
            _ => {
                self.expected_tokens.push(TokenKind::Operator);
                return None;
            }
        }

        match self.consume() {
            Some(Spanned { value: Token::Operator(op), span }) => {
                Some(Spanned::new(op, span))
            }
            _ => {
                panic!("Parser::peek and Parser::consume returned different results");
            }
        }
    }
    
    fn expr_term(&mut self) -> MaybeParsed<Node<Expr>> {
        match self.eat_symbol() {
            Some(Spanned { value, span }) => {
                return MaybeParsed::Ok(Node::new(Expr::Ident(value), span));
            }
            None => { }
        }

        match self.eat_literal() {
            Some(Spanned { value, span }) => {
                return MaybeParsed::Ok(Node::new(Expr::Literal(value), span));
            }
            None => { }
        }

        if self.eat(Token::OpenParen) {
            let start_span = self.previous_span();

            let expr = match self.eat_operator() {
                Some(operator) => {
                    if self.eat(Token::CloseParen) {
                        // got something like (+)
                        Expr::Ident(operator.value)
                    } else {
                        match self.application() {
                            Ok(_expr) => {
                                // got something like (+ a)
                                // TODO: make sectioned operator
                                unimplemented!()
                            }
                            Err(_) => return MaybeParsed::Err(error_expr_node()),
                        }
                    }
                }
                None => {
                    match self.expr(true) {
                        Ok(expr) => Expr::Parenthesised(Box::new(expr)),
                        Err(expr) => return MaybeParsed::Err(expr),
                    }
                }
            };

            if self.eat(Token::CloseParen) {
                let node_span = start_span.merge(&self.previous_span());
                return MaybeParsed::Ok(Node::new(expr, node_span));
            } else {
                // we need to have error node somewhere here
                // so that subsequent passes would ignore this error
                self.emit_error();
                return MaybeParsed::Err(error_expr_node());
            }
        }

        if self.eat(Token::OpenBracket) {
            // TODO: parse list literal
            panic!("list literals are not implemented");
        }

        MaybeParsed::Empty
    }

    fn application(&mut self) -> ParseResult<Node<Expr>> {
        let function = match self.expr_term() {
            MaybeParsed::Ok(expr) => expr,
            MaybeParsed::Empty => {
                self.emit_error();
                return Err(error_expr_node());
            }
            MaybeParsed::Err(expr) => return Err(expr),
        };
        
        let mut args = Vec::new();
        let mut is_ok = true;
        let mut end_span = function.span.clone();

        loop {
            match self.expr_term() {
                MaybeParsed::Ok(expr) => {
                    end_span = expr.span.clone();
                    args.push(expr);
                }
                MaybeParsed::Empty => {
                    break;
                }
                MaybeParsed::Err(expr) => {
                    args.push(expr);
                    is_ok = false;
                    break;
                }
            }
        }

        let node = if args.is_empty() {
            function
        } else {
            let span = function.span.merge(&end_span);
            Node::new(Expr::Apply(Box::new(function), args), span)
        };

        if is_ok {
            Ok(node)
        } else {
            Err(node)
        }
    }

    fn expr_operation(&mut self, mut can_section: bool) -> ParseResult<Node<Expr>> {
        let mut result = match self.application() {
            Ok(expr) => expr,
            Err(expr) => return Err(expr),
        };
        
        loop {
            let operator = match self.eat_operator() {
                Some(operator) => operator,
                None => return Ok(result),
            };

            if can_section && self.peek().map(|x| &x.value) == Some(&Token::CloseParen) {
                // TODO: make lambda from operator
                unimplemented!()
            } else {
                can_section = false;
            }

            match self.application() {
                Ok(rhs) => {
                    let span = result.span.merge(&rhs.span);
                    let rhs = Box::new(rhs);
                    let op = Node::new(operator.value, operator.span);
                    let lhs = Box::new(result);
                    result = Node::new(Expr::Infix(lhs, op, rhs), span);
                }
                Err(rhs) => {
                    let span = result.span.merge(&rhs.span);
                    let rhs = Box::new(rhs);
                    let op = Node::new(operator.value, operator.span);
                    let lhs = Box::new(result);
                    return Err(Node::new(Expr::Infix(lhs, op, rhs), span));
                }
            }
        }
    }

    fn expr(&mut self, can_section: bool) -> ParseResult<Node<Expr>> {
        if self.eat(Token::If) {
            self.if_()
        } else if self.eat(Token::Let) {
            // TODO: parse let
            unimplemented!()
        } else if self.eat(Token::Case) {
            // TODO: parse case
            unimplemented!()
        } else if self.eat(Token::Backslash) {
            // TODO: parse lambda
            unimplemented!()
        } else if self.eat(Token::Do) {
            // TODO: parse do
            unimplemented!()
        } else {
            self.expr_operation(can_section)
        }
    }

    fn if_(&mut self) -> ParseResult<Node<Expr>> {
        let start_span = self.previous_span();

        let condition = try!(self.expr(false));
        
        if !self.eat(Token::Then) {
            self.emit_error();
            return Err(error_expr_node());
        }

        let then_branch = try!(self.expr(false));

        if !self.eat(Token::Else) {
            self.emit_error();
            return Err(error_expr_node());
        }

        let else_branch = try!(self.expr(false));
        let span = start_span.merge(&else_branch.span);
        let expr = Expr::If(
            Box::new(condition),
            Box::new(then_branch),
            Box::new(else_branch));
        Ok(Node::new(expr, span))
    }

    fn pattern_term(&mut self) -> MaybeParsed<Node<Pattern>> {
        if self.eat(Token::Underscore) {
            let span = self.previous_span();
            return MaybeParsed::Ok(Node::new(Pattern::Wildcard, span));
        }

        match self.eat_symbol() {
            Some(Spanned { value, span }) => {
                return MaybeParsed::Ok(Node::new(Pattern::from_symbol(value, span.clone()), span));
            }
            None => { }
        }

        match self.eat_literal() {
            Some(Spanned { value, span }) => {
                return MaybeParsed::Ok(Node::new(Pattern::Literal(value), span));
            }
            None => { }
        }

        if self.eat(Token::OpenParen) {
            let start_span = self.previous_span();
            
            let pattern = match self.pattern() {
                Ok(expr) => Pattern::Parenthesised(Box::new(expr)),
                Err(expr) => return MaybeParsed::Err(expr),
            };
            
            if self.eat(Token::CloseParen) {
                let node_span = start_span.merge(&self.previous_span());
                return MaybeParsed::Ok(Node::new(pattern, node_span));
            } else {
                // we need to have error node somewhere here
                // so that subsequent passes would ignore this error
                self.emit_error();
                return MaybeParsed::Err(error_pattern_node());
            }
        }

        if self.eat(Token::OpenBracket) {
            // TODO: parse list literal
            panic!("list literals are not implemented");
        }

        MaybeParsed::Empty
    }

    fn deconstruct(&mut self) -> ParseResult<Node<Pattern>> {
        let tag_node = match self.eat_symbol() {
            Some(Spanned { value, span }) => {
                match Pattern::from_symbol(value, span.clone()) {
                    p @ Pattern::Var(_) => {
                        return Ok(Node::new(p, span));
                    }
                    Pattern::Deconstruct(ctor, _) => {
                        ctor
                    }
                    p => {
                        panic!("constructed bad pattern from symbol: {:?}", p);
                    }
                }
            }
            None => {
                return match self.pattern_term() {
                    MaybeParsed::Ok(pattern) => Ok(pattern),
                    MaybeParsed::Err(pattern) => Err(pattern),
                    MaybeParsed::Empty => {
                        self.emit_error();
                        Err(error_pattern_node())
                    }
                };
            }
        };

        let mut args = Vec::new();
        let mut end_span = tag_node.span.clone();
        let mut is_ok = true;

        loop {
            match self.pattern_term() {
                MaybeParsed::Ok(pattern) => {
                    end_span = pattern.span.clone();
                    args.push(pattern);
                }
                MaybeParsed::Err(pattern) => {
                    args.push(pattern);
                    is_ok = false;
                    break;
                }
                MaybeParsed::Empty => break,
            }
        }

        let span = tag_node.span.merge(&end_span);
        let pattern = Node::new(Pattern::Deconstruct(tag_node, args), span);
        if is_ok {
            if self.eat(Token::As) {
                match self.eat_symbol() {
                    Some(Spanned { value, span }) => {
                        let node_span = pattern.span.merge(&span);
                        let pattern = Pattern::As(Box::new(pattern), Node::new(value, span));
                        Ok(Node::new(pattern, node_span))
                    }
                    None => {
                        Err(pattern)
                    }
                }
            } else {
                Ok(pattern)
            }
        } else {
            Err(pattern)
        }
    }

    fn pattern(&mut self) -> ParseResult<Node<Pattern>> {
        let mut result = match self.deconstruct() {
            Ok(pattern) => pattern,
            Err(pattern) => return Err(pattern),
        };

        loop {
            let operator = match self.eat_operator() {
                Some(operator) => operator,
                None => return Ok(result),
            };

            match self.deconstruct() {
                Ok(rhs) => {
                    let span = result.span.merge(&rhs.span);
                    let rhs = Box::new(rhs);
                    let op = Node::new(operator.value, operator.span);
                    let lhs = Box::new(result);
                    result = Node::new(Pattern::Infix(lhs, op, rhs), span);
                }
                Err(rhs) => {
                    let span = result.span.merge(&rhs.span);
                    let rhs = Box::new(rhs);
                    let op = Node::new(operator.value, operator.span);
                    let lhs = Box::new(result);
                    return Err(Node::new(Pattern::Infix(lhs, op, rhs), span));
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use parsing::lexer::lex;
    use ast::{Expr, Pattern, Literal};
    use super::Parser;

    fn write_literal(literal: &Literal, output: &mut String) {
        match *literal {
            Literal::Bool(value) => {
                if value {
                    output.push_str("true");
                } else {
                    output.push_str("false");
                }
            }
            Literal::Char(ch) => {
                output.push('\'');
                output.push(ch);
                output.push('\'');
            }
            Literal::Float(f) => {
                output.push_str(&f.to_string());
            }
            Literal::Int(i) => {
                output.push_str(&i.to_string());
            }
            Literal::Str(ref s) => {
                output.push('"');
                output.push_str(&s);
                output.push('"');
            }
        }
    }

    fn write_expr(expr: &Expr, output: &mut String) {
        match *expr {
            Expr::Apply(ref f, ref args) => {
                output.push_str("(apply ");
                write_expr(&f.node, output);
                for arg in args {
                    output.push(' ');
                    write_expr(&arg.node, output);
                }
                output.push(')');
            }
            Expr::Error => {
                panic!("found error node");
            }
            Expr::Ident(ref ident) => {
                output.push_str(&ident);
            }
            Expr::If(ref cond, ref then, ref else_) => {
                output.push_str("(if ");
                write_expr(&cond.node, output);
                output.push(' ');
                write_expr(&then.node, output);
                output.push(' ');
                write_expr(&else_.node, output);
                output.push(')');
            }
            Expr::Infix(ref lhs, ref op, ref rhs) => {
                output.push('(');
                output.push_str(&op.node);
                output.push(' ');
                write_expr(&lhs.node, output);
                output.push(' ');
                write_expr(&rhs.node, output);
                output.push(')');
            }
            Expr::Literal(ref literal) => {
                write_literal(literal, output);
            }
            Expr::Parenthesised(ref expr) => {
                output.push_str("(parens ");
                write_expr(&expr.node, output);
                output.push_str(")");
            }
        }
    }

    fn write_pattern(pattern: &Pattern, output: &mut String) {
        match *pattern {
            Pattern::Wildcard => {
                output.push('_');
            }
            Pattern::Var(ref var) => {
                output.push_str("(var ");
                output.push_str(&var);
                output.push_str(")");
            }
            Pattern::Deconstruct(ref tag, ref args) => {
                output.push_str("(dec ");
                output.push_str(&tag.node);
                for arg in args {
                    output.push(' ');
                    write_pattern(&arg.node, output);
                }
                output.push(')');
            }
            Pattern::Error => {
                panic!("found error pattern node");
            }
            Pattern::Infix(ref lhs, ref op, ref rhs) => {
                output.push_str("(opdec ");
                output.push_str(&op.node);
                output.push(' ');
                write_pattern(&lhs.node, output);
                output.push(' ');
                write_pattern(&rhs.node, output);
                output.push(')');
            }
            Pattern::Literal(ref literal) => {
                write_literal(literal, output);
            }
            Pattern::Parenthesised(ref pattern) => {
                output.push_str("(parens ");
                write_pattern(&pattern.node, output);
                output.push_str(")");
            }
            Pattern::As(ref pattern, ref alias) => {
                output.push_str("(alias ");
                output.push_str(&alias.node);
                output.push_str(" ");
                write_pattern(&pattern.node, output);
                output.push_str(")");
            }
        }
    }

    fn check_expr(source: &str, expected: &str) {
        let tokens = lex(source);
        let mut parser = Parser::new(tokens.into_iter());
        let expr = parser.expr(false).ok().unwrap();
        let mut printed = String::new();
        write_expr(&expr.node, &mut printed);
        assert_eq!(expected, printed);
    }

    fn check_pattern(source: &str, expected: &str) {
        let tokens = lex(source);
        let mut parser = Parser::new(tokens.into_iter());
        let pattern = parser.pattern().ok().unwrap();
        let mut printed = String::new();
        write_pattern(&pattern.node, &mut printed);
        assert_eq!(expected, printed);
    }

    #[test]
    fn basic_expr() {
        check_expr(
            "(f (g 4 true))",
            "(parens (apply f (parens (apply g 4 true))))")
    }

    #[test]
    fn infix_operator() {
        check_expr(
            "a + 4",
            "(+ a 4)");
    }

    #[test]
    fn if_expr() {
        check_expr(
            "if a > b then a else b",
            "(if (> a b) a b)");
    }

    #[test]
    fn basic_pattern() {
        check_pattern(
            "Some a",
            "(dec Some (var a))");
    }

    #[test]
    fn basic_pattern_2() {
        check_pattern(
            "Triple _ a (Pair 1 _)",
            "(dec Triple _ (var a) (parens (dec Pair 1 _)))");
    }
    
    #[test]
    fn basic_pattern_3() {
        check_pattern(
            "Triple _ a Pair 1 _",
            "(dec Triple _ (var a) (dec Pair) 1 _)");
    }

    #[test]
    fn infix_pattern() {
        check_pattern(
            "Pair a b :: rest",
            "(opdec :: (dec Pair (var a) (var b)) (var rest))");
    }

    #[test]
    fn parenthesised_var_pattern() {
        check_pattern("(a)", "(parens (var a))")
    }
    
    #[test]
    fn alias_pattern() {
        check_pattern(
            "Some x as opt",
            "(alias opt (dec Some (var x)))")
    }
}
