use std::cmp::Ordering;
use std::iter::Peekable;
use errors::ParseError;
use position::{Position, Span, Spanned, DUMMY_SPAN};
use parsing::tokens::{Token, TokenKind};
use ast::{
    Expr, Literal, Pattern, CaseBranch, Node, LetDecl, Decl, Def, TypeAnnot,
    Associativity, Scheme, Type, TypeAlias, UnionType, UnionCase, RecordType,
    RawSymbol, Trait, Impl, ListItem, ExposedItem, ExposedSubitem, ModuleDef,
    Import, Module,
};


pub fn parse_module<I>(tokens: I, require_def: bool) -> (Option<Module<RawSymbol>>, Vec<ParseError>)
        where I: Iterator<Item=Spanned<Token>> {
    let mut parser = Parser::new(tokens);
    let module = parser.module(require_def).ok();
    debug_assert!(parser.is_at_end());

    (module, parser.errors)
}

type ExprNode = Node<Expr<RawSymbol>>;
type PatternNode = Node<Pattern<RawSymbol>>;
type TypeNode = Node<Type<RawSymbol>>;

type ParseResult<T> = Result<T, ()>;

struct Parser<I: Iterator<Item=Spanned<Token>>> {
    next_token: Option<Spanned<Token>>,
    tokens: Peekable<I>,
    expected_tokens: Vec<TokenKind>,
    errors: Vec<ParseError>,
    current_indent: usize,
    accept_aligned: bool,
    last_token_span: Span,
}

impl<I: Iterator<Item=Spanned<Token>>> Parser<I> {
    fn new(mut tokens: I) -> Parser<I> {
        let next_token = tokens.next();
        Parser {
            next_token: next_token,
            tokens: tokens.peekable(),
            expected_tokens: Vec::new(),
            errors: Vec::new(),
            current_indent: 0,
            accept_aligned: false,
            last_token_span: DUMMY_SPAN.clone(),
        }
    }

    fn emit_error(&mut self) {
        let (token, span) = match self.next_token {
            // don't report errors on error tokens,
            // these will be reported by lexer
            Some(Spanned { value: Token::Error, .. }) => return,
            Some(Spanned { ref value, ref span }) => (value, span.clone()),
            _ => panic!("skipped EndOfInput token"),
        };

        self.expected_tokens.sort_by_key(TokenKind::to_string);
        self.expected_tokens.dedup();

        let any_matches = self.expected_tokens.iter().any(|kind| kind.matches_token(token));

        let mut message = if self.expected_tokens.len() == 0 {
            panic!("no tokens expected but got error");
        } else if self.expected_tokens.len() == 1 {
            format!("Expected {}", self.expected_tokens[0].to_string())
        } else {
            let mut message = "Expected one of: ".to_string();
            let mut add_comma = false;
            for token in &self.expected_tokens {
                if add_comma {
                    message.push_str(", ");
                }
                message.push_str(&token.to_string());
                add_comma = true;
            }
            message
        };

        message.push('.');

        if any_matches {
            message.push_str(" Make sure that your code is properly indented.");
        }

        self.errors.push(ParseError {
            message: message,
            span: span,
        });
    }

    fn previous_span(&self) -> Span {
        self.last_token_span.clone()
    }

    fn next_span(&self) -> Span {
        match self.next_token {
            Some(Spanned { ref span, .. }) => span.clone(),
            _ => panic!("skipped EndOfInput token"),
        }
    }

    fn align_on_next(&mut self) -> usize {
        let token_column = self.peek()
            .map(|s| s.span.start.column)
            .unwrap_or(self.current_indent);

        let old_indent = self.current_indent;
        self.current_indent = token_column;
        old_indent
    }

    fn skip_to_aligned(&mut self) -> bool {
        let indent = self.current_indent;
        // skip at least one to make sure we do not get stuck on aligned error
        self.consume();
        loop {
            match self.next_token {
                Some(Spanned { value: Token::EndOfInput, .. }) => {
                    return false;
                }
                Some(Spanned { ref span, .. }) => {
                    let column = span.start.column;
                    if column < indent {
                        return false;
                    } else if column == indent {
                        return true;
                    }
                }
                None => {
                    panic!("parser skipped EndOfInput token");
                }
            }

            self.consume();
        }
    }

    fn is_at_end(&self) -> bool {
        match self.next_token {
            Some(ref token) => token.value == Token::EndOfInput,
            None => panic!("skipped EndOfInput token"),
        }
    }

    fn peek(&self) -> Option<&Spanned<Token>> {
        match self.next_token {
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

    fn peek2(&mut self) -> Option<&Spanned<Token>> {
        // next token might be too much to the left or missing altogehter
        // so first we have to make sure it is there
        if self.peek().is_none() {
            return None;
        }

        match self.tokens.peek() {
            Some(ref token) => {
                if token.span.start.column > self.current_indent {
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
        let tok = self.next_token.take();
        self.next_token = self.tokens.next();
        tok
    }

    fn expect(&mut self, expected: Token) -> ParseResult<()> {
        if self.eat(expected) {
            Ok(())
        } else {
            self.emit_error();
            Err(())
        }
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

    fn eat_symbol(&mut self) -> Option<Spanned<RawSymbol>> {
        self.expected_tokens.push(TokenKind::Ident);
        match self.peek() {
            Some(&Spanned { value: Token::Ident(_), .. }) => { }
            _ => return None,
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
    
    fn eat_unqualified_name(&mut self) -> Option<Spanned<String>> {
        self.expected_tokens.push(TokenKind::Ident);
        match self.peek() {
            Some(&Spanned { value: Token::Ident(RawSymbol::Unqualified(_)), .. }) => { }
            _ => return None,
        }

        match self.consume() {
            Some(Spanned { value: Token::Ident(RawSymbol::Unqualified(name)), span }) => {
                Some(Spanned::new(name, span))
            }
            _ => {
                panic!("Parser::peek and Parser::consume returned different results");
            }
        }
    }

    fn eat_var_name(&mut self) -> Option<Spanned<String>> {
        self.expected_tokens.push(TokenKind::VarName);
        match self.peek() {
            Some(&Spanned { value: Token::Ident(RawSymbol::Unqualified(ref name)), .. }) => {
                if name.chars().nth(0).unwrap().is_uppercase() {
                    return None;
                }
            }
            _ => {
                return None;
            }
        }

        match self.consume() {
            Some(Spanned { value: Token::Ident(RawSymbol::Unqualified(name)), span }) => {
                debug_assert!(!name.chars().nth(0).unwrap().is_uppercase());
                Some(Spanned::new(name, span))
            }
            _ => {
                panic!("Parser::peek and Parser::consume returned different results");
            }
        }
    }
    
    fn eat_literal(&mut self) -> Option<Spanned<Literal>> {
        self.expected_tokens.push(TokenKind::Literal);
        match self.peek() {
            Some(&Spanned { value: Token::Int(_), .. }) |
            Some(&Spanned { value: Token::Float(_), .. }) |
            Some(&Spanned { value: Token::Bool(_), .. }) => { }
            _ => return None,
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
        self.expected_tokens.push(TokenKind::Operator);
        match self.peek() {
            Some(&Spanned { value: Token::Operator(_), .. }) => { }
            _ => return None,
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
    
    fn expr_term(&mut self) -> Option<ParseResult<ExprNode>> {
        match self.eat_symbol() {
            Some(Spanned { value, span }) => {
                return Some(Ok(Node::new(Expr::Ident(value), span)));
            }
            None => { }
        }

        match self.eat_literal() {
            Some(Spanned { value, span }) => {
                return Some(Ok(Node::new(Expr::Literal(value), span)));
            }
            None => { }
        }

        if self.eat(Token::OpenParen) {
            let start_span = self.previous_span();

            let expr = match self.eat_operator() {
                Some(operator) => {
                    return if self.eat(Token::CloseParen) {
                        // got something like (+)
                        let span = start_span.merge(&self.previous_span());
                        let symbol = RawSymbol::Unqualified(operator.value);
                        Some(Ok(Node::new(Expr::Ident(symbol), span)))
                    } else {
                        match self.application() {
                            Ok(expr) => {
                                if !self.eat(Token::CloseParen) {
                                    self.emit_error();
                                    return Some(Err(()));
                                }
                                let Spanned { value, span } = operator;
                                let operator = Node::new(value, span);
                                let span = start_span.merge(&self.previous_span());
                                // got something like (+ a)
                                Some(Ok(operator_right_section(operator, expr, span)))
                            }
                            Err(_) => Some(Err(())),
                        }
                    };
                }
                None => {
                    match self.expr(true) {
                        Ok(expr) => expr,
                        Err(_) => return Some(Err(())),
                    }
                }
            };

            if self.eat(Token::CloseParen) {
                let node_span = start_span.merge(&self.previous_span());
                let expr = Expr::Parenthesised(Box::new(expr));
                return Some(Ok(Node::new(expr, node_span)));
            } else if self.eat(Token::Comma) {
                // TODO: parse tuple
                unimplemented!()
            } else {
                self.emit_error();
                return Some(Err(()));
            }
        }

        if self.eat(Token::OpenBracket) {
            // TODO: parse list literal
            panic!("list literals are not implemented");
        }

        None
    }

    fn application(&mut self) -> ParseResult<ExprNode> {
        let function = match self.expr_term() {
            Some(Ok(expr)) => expr,
            Some(Err(_)) => return Err(()),
            None => {
                self.emit_error();
                return Err(());
            }
        };
        
        let mut args = Vec::new();
        let mut span = function.span.clone();

        while let Some(result) = self.expr_term() {
            let expr = try!(result);
            span = span.merge(&expr.span);
            args.push(expr);
        }

        let node = if args.is_empty() {
            function
        } else {
            Node::new(Expr::Apply(Box::new(function), args), span)
        };

        Ok(node)
    }

    fn expr_operation(&mut self, mut can_section: bool) -> ParseResult<ExprNode> {
        let start_span = self.previous_span();
        let mut result = try!(self.application());
        
        loop {
            let operator = match self.eat_operator() {
                Some(operator) => operator,
                None => return Ok(result),
            };

            if can_section && self.peek().map(|x| &x.value) == Some(&Token::CloseParen) {
                // got something like (a +)
                let span = start_span.merge(&self.next_span());
                let operator = Node::new(operator.value, operator.span);
                return Ok(operator_left_section(operator, result, span));
            } else {
                can_section = false;
            }

            let rhs = try!(self.application());
            let span = result.span.merge(&rhs.span);
            let rhs = Box::new(rhs);
            let op = Node::new(RawSymbol::Unqualified(operator.value), operator.span);
            let lhs = Box::new(result);
            result = Node::new(Expr::Infix(lhs, op, rhs), span);
        }
    }

    fn expr(&mut self, can_section: bool) -> ParseResult<ExprNode> {
        if self.eat(Token::If) {
            self.if_()
        } else if self.eat(Token::Let) {
            self.let_()
        } else if self.eat(Token::Case) {
            self.case()
        } else if self.eat(Token::Backslash) {
            self.lambda()
        } else if self.eat(Token::Do) {
            self.do_expr()
        } else {
            self.expr_operation(can_section)
        }
    }

    fn pattern_term(&mut self) -> Option<ParseResult<PatternNode>> {
        if self.eat(Token::Underscore) {
            let span = self.previous_span();
            return Some(Ok(Node::new(Pattern::Wildcard, span)));
        }

        match self.eat_symbol() {
            Some(Spanned { value, span }) => {
                return Some(Ok(Node::new(Pattern::from_symbol(value, span.clone()), span)));
            }
            None => { }
        }

        match self.eat_literal() {
            Some(Spanned { value, span }) => {
                return Some(Ok(Node::new(Pattern::Literal(value), span)));
            }
            None => { }
        }

        if self.eat(Token::OpenParen) {
            let start_span = self.previous_span();
            
            let pattern = match self.pattern() {
                Ok(pattern) => pattern,
                Err(_) => return Some(Err(())),
            };
            
            if self.eat(Token::CloseParen) {
                let node_span = start_span.merge(&self.previous_span());
                let pattern = Pattern::Parenthesised(Box::new(pattern));
                return Some(Ok(Node::new(pattern, node_span)));
            } else if self.eat(Token::Comma) {
                // TODO: parse tuple pattern
                unimplemented!()
            } else {
                self.emit_error();
                return Some(Err(()));
            }
        }

        if self.eat(Token::OpenBracket) {
            // TODO: parse list pattern
            panic!("list patterns are not implemented");
        }

        None
    }

    fn deconstruct(&mut self) -> ParseResult<PatternNode> {
        let tag_node = match self.eat_symbol() {
            Some(Spanned { value, span }) => {
                match Pattern::from_symbol(value, span.clone()) {
                    p@Pattern::Var(_) => {
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
                    Some(Ok(pattern)) => Ok(pattern),
                    Some(Err(_)) => Err(()),
                    None => {
                        self.emit_error();
                        Err(())
                    }
                };
            }
        };

        let mut args = Vec::new();
        let mut span = tag_node.span.clone();

        while let Some(result) = self.pattern_term() {
            let pattern = try!(result);
            span = span.merge(&pattern.span);
            args.push(pattern);
        }

        let pattern = Node::new(Pattern::Deconstruct(tag_node, args), span);
        if self.eat(Token::As) {
            match self.eat_unqualified_name() {
                Some(Spanned { value, span }) => {
                    let node_span = pattern.span.merge(&span);
                    let pattern = Pattern::As(Box::new(pattern), Node::new(value, span));
                    Ok(Node::new(pattern, node_span))
                }
                None => {
                    Err(())
                }
            }
        } else {
            Ok(pattern)
        }
    }

    fn pattern(&mut self) -> ParseResult<PatternNode> {
        let mut result = try!(self.deconstruct());

        loop {
            let operator = match self.eat_operator() {
                Some(operator) => operator,
                None => return Ok(result),
            };

            let rhs = Box::new(try!(self.deconstruct()));
            let span = result.span.merge(&rhs.span);
            let op = Node::new(RawSymbol::Unqualified(operator.value), operator.span);
            let lhs = Box::new(result);
            result = Node::new(Pattern::Infix(lhs, op, rhs), span);    
        }
    }
    
    fn if_(&mut self) -> ParseResult<ExprNode> {
        let start_span = self.previous_span();

        let condition = try!(self.expr(false));
        
        try!(self.expect(Token::Then));

        let then_branch = try!(self.expr(false));

        try!(self.expect(Token::Else));

        let else_branch = try!(self.expr(false));
        let span = start_span.merge(&else_branch.span);
        let expr = Expr::If(
            Box::new(condition),
            Box::new(then_branch),
            Box::new(else_branch));
        Ok(Node::new(expr, span))
    }

    fn lambda(&mut self) -> ParseResult<ExprNode> {
        let start_span = self.previous_span();

        let mut params = Vec::new();
        while let Some(result) = self.pattern_term() {
            params.push(try!(result));
        }

        if params.is_empty() {
            self.emit_error();
            return Err(());
        }

        try!(self.expect(Token::Arrow));

        match self.expr(false) {
            Ok(expr) => {
                let span = start_span.merge(&expr.span);
                Ok(Node::new(Expr::Lambda(params, Box::new(expr)), span))
            }
            Err(_) => {
                Err(())
            }
        }
    }

    fn case(&mut self) -> ParseResult<ExprNode> {
        let mut span = self.previous_span();

        let value = try!(self.expr(false));

        try!(self.expect(Token::Of));

        let old_indent = self.align_on_next();
        let mut branches = Vec::new();

        loop {
            self.accept_aligned = true;

            if self.peek().is_none() {
                if branches.is_empty() {
                    // do this to fill wanted token list
                    debug_assert!(self.case_branch().is_err());
                    self.emit_error();
                    self.current_indent = old_indent;
                    self.accept_aligned = false;
                    return Err(());
                } else {
                    break;
                }
            }

            match self.case_branch() {
                Ok(branch) => {
                    span = span.merge(&branch.span);
                    branches.push(branch);
                }
                Err(_) => {
                    if !self.skip_to_aligned() {
                        break;
                    }
                }
            }
        }

        self.current_indent = old_indent;
        self.accept_aligned = false;

        Ok(Node::new(Expr::Case(Box::new(value), branches), span))
    }

    fn case_branch(&mut self) -> ParseResult<Node<CaseBranch<RawSymbol>>> {
        self.accept_aligned = true;
        
        let pattern = try!(self.pattern());

        let guard = if self.eat(Token::If) {
            Some(try!(self.expr(false)))
        } else {
            None
        };

        try!(self.expect(Token::Arrow));

        match self.expr(false) {
            Ok(expr) => {
                let span = pattern.span.merge(&expr.span);
                Ok(Node::new(CaseBranch {
                    pattern: pattern,
                    value: expr,
                    guard: guard,
                }, span))
            }
            Err(_) => Err(()),
        }
    }

    fn let_(&mut self) -> ParseResult<ExprNode> {
        let old_indent = self.align_on_next();
        let mut decls = Vec::new();
        let mut span = self.previous_span();

        loop {
            self.accept_aligned = true;

            if self.peek().is_none() || self.peek().map(|t| &t.value) == Some(&Token::In) {
                if decls.is_empty() {
                    // do this to fill wanted token list
                    debug_assert!(!self.let_decl(true).is_ok());
                    self.emit_error();
                    self.current_indent = old_indent;
                    self.accept_aligned = false;
                    return Err(());
                } else {
                    break;
                }
            }

            match self.let_decl(true) {
                Ok(decl) => {
                    span = span.merge(&decl.span);
                    decls.push(decl);
                }
                Err(_) => {
                    if !self.skip_to_aligned() {
                        break;
                    }
                }
            }
        }

        self.current_indent = old_indent;
        self.accept_aligned = false;

        try!(self.expect(Token::In));

        match self.expr(false) {
            Ok(expr) => {
                span = span.merge(&expr.span);
                Ok(Node::new(Expr::Let(decls, Box::new(expr)), span))
            }
            Err(_) => {
                Err(())
            }
        }
    }

    fn let_decl(&mut self, allow_type: bool) -> ParseResult<Node<LetDecl<RawSymbol>>> {
        let is_paren = match self.peek() {
            Some(&Spanned { value: Token::OpenParen, .. }) => true,
            _ => false,
        };
        let is_op = match self.peek2() {
            Some(&Spanned { value: Token::Operator(_), .. }) => true,
            _ => false,
        };

        if is_paren && is_op {
            assert!(self.eat(Token::OpenParen));
            let symbol = match self.consume() {
                Some(Spanned { value: Token::Operator(op), span }) => {
                    Node::new(op, span)
                }
                _ => {
                    panic!("expected to get operator");
                }
            };

            if self.eat(Token::CloseParen) {
                self.type_or_def(symbol, allow_type)
            } else {
                self.emit_error();
                Err(())
            }
        } else if is_paren || is_op {
            self.pattern_assign().map(|n| n.map(LetDecl::Def))
        } else {
            match self.eat_var_name() {
                Some(Spanned { value, span }) => {
                    self.type_or_def(Node::new(value, span), allow_type)
                }
                None => {
                    debug_assert!(!self.eat(Token::OpenParen));
                    self.emit_error();
                    Err(())
                }
            }
        }
    }

    fn scheme(&mut self) -> ParseResult<Node<Scheme<RawSymbol>>> {
        if self.eat(Token::OpenParen) {
            let mut constraints = Vec::new();
            loop {
                let var = match self.eat_var_name() {
                    Some(Spanned { value, span }) => Node::new(value, span),
                    None => return Err(()),
                };
                try!(self.expect(Token::Colon));
                let type_ = try!(self.type_());
                if self.eat(Token::CloseParen) {
                    break;
                } else {
                    try!(self.expect(Token::Comma));
                }
                constraints.push((var, type_));
            }
            try!(self.expect(Token::FatArrow));
            match self.type_() {
                Ok(type_) => {
                    let span = constraints[0].0.span.merge(&type_.span);
                    let scheme = Scheme {
                        vars: constraints,
                        type_: type_,
                    };
                    Ok(Node::new(scheme, span))
                }
                Err(_) => Err(()),
            }
        } else {
            self.type_().map(|type_| {
                let span = type_.span.clone();
                let scheme = Scheme {
                    vars: Vec::new(),
                    type_: type_,
                };
                Node::new(scheme, span)
            })
        }
    }

    fn type_term(&mut self) -> Option<ParseResult<TypeNode>> {
        match self.eat_symbol() {
            Some(Spanned { value, span }) => {
                return Some(Ok(Node::new(Type::from_symbol(value), span)));
            }
            _ => { }
        }

        if self.eat(Token::OpenParen) {
            let type_ = match self.type_() {
                Ok(type_) => type_,
                Err(()) => return Some(Err(())),
            };

            if self.eat(Token::Comma) {
                // TODO: parse tuple type
                unimplemented!()
            } else if self.eat(Token::CloseParen) {
                Some(Ok(type_))
            } else {
                self.emit_error();
                Some(Err(()))
            }
        } else {
            None
        }
    }

    fn type_application(&mut self) -> ParseResult<TypeNode> {
        let mut result = match self.type_term() {
            Some(Ok(type_)) => type_,
            Some(Err(_)) | None => return Err(()),
        };

        while let Some(res) = self.type_term() {
            let type_ = try!(res);
            let span = result.span.merge(&type_.span);
            result = Node::new(Type::Apply(Box::new(result), Box::new(type_)), span);
        }

        Ok(result)
    }

    fn type_(&mut self) -> ParseResult<TypeNode> {
        let arg = try!(self.type_application());

        if self.eat(Token::Arrow) {
            match self.type_() {
                Ok(result) => {
                    let span = arg.span.merge(&result.span);
                    Ok(Node::new(Type::Function(Box::new(arg), Box::new(result)), span))
                }
                Err(()) => {
                    Err(())
                }
            }
        } else {
            Ok(arg)
        }
    }

    fn pattern_assign(&mut self) -> ParseResult<Node<Def<RawSymbol>>> {
        let first = match self.pattern_term() {
            Some(Ok(pattern)) => pattern,
            Some(Err(_)) | None => return Err(()),
        };

        if self.eat(Token::Equals) {
            match self.expr(false) {
                Ok(expr) => {
                    let span = first.span.merge(&expr.span);
                    Ok(Node::new(Def {
                        pattern: first,
                        value: Some(expr),
                    }, span))
                }
                Err(()) => {
                    let span = first.span.clone();
                    Ok(Node::new(Def {
                        pattern: first,
                        value: None,
                    }, span))
                }
            }
        } else {
            let (op, second) = match self.eat_operator() {
                Some(Spanned { value, span }) => {
                    match self.pattern_term() {
                        Some(Ok(pattern)) => (Node::new(value, span), pattern),
                        Some(Err(_)) | None => return Err(()),
                    }
                }
                None => return Err(()),
            };

            try!(self.expect(Token::Equals));

            let pattern = op.map(Pattern::Var);
            let params = vec![first, second];
            let span;

            let expr = match self.expr(false) {
                Ok(expr) => {
                    let lambda_span = params[0].span.merge(&expr.span);
                    span = lambda_span.clone();
                    Some(Node::new(Expr::Lambda(params, Box::new(expr)), lambda_span))
                }
                Err(_) => {
                    span = pattern.span.clone();
                    None
                }
            };
            
            Ok(Node::new(Def {
                pattern: pattern,
                value: expr,
            }, span))
        }
    }

    fn type_or_def(&mut self, symbol: Node<String>, allow_type: bool) -> ParseResult<Node<LetDecl<RawSymbol>>> {
        if allow_type && self.eat(Token::Colon) {
            match self.scheme() {
                Ok(scheme) => {
                    let node_span = symbol.span.merge(&scheme.span);
                    Ok(Node::new(LetDecl::Type(TypeAnnot {
                        value: symbol,
                        type_: scheme,
                    }), node_span))
                }
                Err(_) => {
                    Err(())
                }
            }
        } else {
            let mut params = Vec::new();
            let pattern = symbol.map(Pattern::Var);
            loop {
                match self.pattern_term() {
                    Some(Ok(pattern)) => params.push(pattern),
                    Some(Err(_)) => {
                        let span = pattern.span.clone();
                        let def = LetDecl::Def(Def {
                            pattern: pattern,
                            value: None,
                        });
                        return Ok(Node::new(def, span));
                    }
                    None => {
                        if self.eat(Token::Equals) {
                            break;
                        } else {
                            let span = pattern.span.clone();
                            let def = LetDecl::Def(Def {
                                pattern: pattern,
                                value: None,
                            });
                            return Ok(Node::new(def, span));
                        }
                    }
                }
            }
            
            let span;
            let expr = match self.expr(false) {
                Ok(expr) => {
                    span = pattern.span.merge(&expr.span);
                    if params.is_empty() {
                        Some(expr)
                    } else {
                        let lambda_span = params[0].span.merge(&expr.span);
                        Some(Node::new(Expr::Lambda(params, Box::new(expr)), lambda_span))
                    }
                }
                Err(_) => {
                    span = pattern.span.clone();
                    None
                }
            };
            
            Ok(Node::new(LetDecl::Def(Def {
                pattern: pattern,
                value: expr,
            }), span))
        }
    }

    fn do_expr(&mut self) -> ParseResult<ExprNode> {
        let old_indent = self.align_on_next();

        match self.do_statement() {
            Some(Ok(expr)) => {
                self.current_indent = old_indent;
                Ok(expr)
            }
            Some(Err(_)) => {
                self.current_indent = old_indent;
                Err(())
            }
            None => {
                self.emit_error();
                self.current_indent = old_indent;
                Err(())
            }
        }
    }

    fn do_statement(&mut self) -> Option<ParseResult<ExprNode>> {
        self.accept_aligned = true;
        if self.eat(Token::Let) {
            return Some(self.do_let());
        } else if self.eat(Token::If) {
            return Some(self.do_if());
        }
        self.expected_tokens.push(TokenKind::Token(Token::OpenParen));
        self.expected_tokens.push(TokenKind::Ident);
        self.expected_tokens.push(TokenKind::VarName);
        if self.peek().is_none() {
            return None;
        }
        if self.peek().map(|t| &t.value) == Some(&Token::OpenParen) ||
            self.peek2().map(|t| &t.value) == Some(&Token::BackArrow) {
            // pattern <- expr
            match self.pattern() {
                Ok(pattern) => {
                    if !self.eat(Token::BackArrow) {
                        self.emit_error();
                        return Some(Err(()));
                    }
                    let arrow_span = self.previous_span();
                    match self.expr(false) {
                        Ok(expr) => {
                            let rest = match self.do_statement() {
                                Some(Ok(rest)) => rest,
                                Some(Err(_)) => {
                                    return Some(Err(()));
                                }
                                None => {
                                    self.emit_error();
                                    return Some(Err(()));
                                }
                            };
                            let apply_span = pattern.span.merge(&rest.span);
                            let args = vec![pattern];
                            let lambda_span = rest.span.clone();
                            let lambda = Node::new(Expr::Lambda(args, Box::new(rest)), lambda_span);
                            let bind = RawSymbol::Trusted("Core.Monad".to_string(), "bind".to_string());
                            let bind_node = Node::new(Expr::Ident(bind), arrow_span);
                            let apply = Node::new(Expr::Apply(Box::new(bind_node), vec![expr, lambda]), apply_span);
                            Some(Ok(apply))
                        }
                        Err(_) => {
                            Some(Err(()))
                        }
                    }
                }
                Err(_) => {
                    Some(Err(()))
                }
            }
        } else {
            match self.expr(false) {
                Ok(expr) => {
                    let rest = match self.do_statement() {
                        Some(Ok(expr)) => expr,
                        Some(Err(_)) => return Some(Err(())),
                        None => return Some(Ok(expr)),
                    };
                    let apply_span = expr.span.merge(&rest.span);
                    let args = vec![Node::new(Pattern::Wildcard, DUMMY_SPAN.clone())];
                    let lambda_span = rest.span.clone();
                    let lambda = Node::new(Expr::Lambda(args, Box::new(rest)), lambda_span);
                    let bind = RawSymbol::Trusted("Core.Monad".to_string(), "bind".to_string());
                    let bind_node = Node::new(Expr::Ident(bind), DUMMY_SPAN.clone());
                    let apply = Node::new(Expr::Apply(Box::new(bind_node), vec![expr, lambda]), apply_span);
                    Some(Ok(apply))
                }
                Err(_) => {
                    Some(Err(()))
                }
            }
        }
    }

    fn do_if(&mut self) -> ParseResult<ExprNode> {
        let if_span = self.previous_span();

        let cond = try!(self.expr(false));

        let rest = match self.do_statement() {
            Some(Ok(rest)) => rest,
            Some(Err(_)) => return Err(()),
            None => {
                self.emit_error();
                return Err(());
            }
        };

        let empty = RawSymbol::Trusted("Core.Monad".to_string(), "empty".to_string());
        let empty_node = Node::new(Expr::Ident(empty), DUMMY_SPAN.clone());
        let span = if_span.merge(&rest.span);
        let if_ = Expr::If(Box::new(cond), Box::new(rest), Box::new(empty_node));
        Ok(Node::new(if_, span))
    }

    fn do_let(&mut self) -> ParseResult<ExprNode> {
        let let_span = self.previous_span();

        let def = try!(self.pattern_assign()).map(LetDecl::Def);

        let rest = match self.do_statement() {
            Some(Ok(rest)) => rest,
            Some(Err(_)) => return Err(()),
            None => {
                self.emit_error();
                return Err(());
            }
        };

        let span = let_span.merge(&rest.span);
        let e = Expr::Let(vec![def], Box::new(rest));
        Ok(Node::new(e, span))
    }

    fn fixity(&mut self, associativity: Associativity) -> ParseResult<Node<Decl<RawSymbol>>> {
        let span = self.previous_span();

        let op = match self.eat_operator() {
            Some(Spanned { value, span }) => Node::new(value, span),
            None => {
                self.emit_error();
                return Err(());
            }
        };

        match self.peek() {
            Some(&Spanned { value: Token::Int(_), .. }) => { }
            _ => {
                self.expected_tokens.push(TokenKind::Int);
                return Err(());
            }
        }

        let precedence = match self.consume() {
            Some(Spanned { value: Token::Int(precedence), span }) => {
                Node::new(precedence, span)
            }
            _ => {
                panic!("Parser::peek and Parser::consume returned different reults");
            }
        };

        let span = span.merge(&self.previous_span());

        Ok(Node::new(Decl::Infix(associativity, op, precedence), span))
    }

    fn type_decl(&mut self) -> ParseResult<Node<Decl<RawSymbol>>> {
        let span = self.previous_span();

        let is_alias = self.eat(Token::Alias);

        let name = match self.eat_unqualified_name() {
            Some(Spanned { value, span }) => Node::new(value, span),
            None => return Err(()),
        };

        let mut vars = Vec::new();
        while let Some(Spanned { value, span }) = self.eat_var_name() {
            vars.push(Node::new(value, span));
        }

        try!(self.expect(Token::Equals));

        if is_alias {
            let type_ = self.type_().ok();
            let span = span.merge(&self.previous_span());
            Ok(Node::new(Decl::TypeAlias(TypeAlias {
                name: name,
                vars: vars,
                type_: type_,
            }), span))
        } else if self.eat(Token::OpenBrace) {
            let fields = self.record().unwrap_or_else(|_| Vec::new());
            let span = span.merge(&self.previous_span());
            Ok(Node::new(Decl::Record(RecordType {
                name: name,
                vars: vars,
                fields: fields,
            }), span))
        } else {
            // allow pipe before first case, but not mandatory
            self.eat(Token::Pipe);
            let mut cases = Vec::new();
            loop {
                match self.union_case() {
                    Ok(case) => {
                        cases.push(case);
                        if !self.eat(Token::Pipe) {
                            break;
                        }
                    }
                    Err(_) => {
                        cases.clear();
                        break;
                    }
                }
            }
            let span = span.merge(&self.previous_span());
            Ok(Node::new(Decl::Union(UnionType {
                name: name,
                vars: vars,
                cases: cases,
            }), span))
        }
    }

    fn record(&mut self) -> ParseResult<Vec<(Node<String>, Node<Type<RawSymbol>>)>> {
        let mut fields = Vec::new();
        while !self.eat(Token::CloseBrace) {
            let name = match self.eat_unqualified_name() {
                Some(Spanned { value, span }) => Node::new(value, span),
                None => return Err(()),
            };
            try!(self.expect(Token::Colon));
            let type_ = try!(self.type_());
            fields.push((name, type_));
            if !self.eat(Token::Comma) {
                if !self.eat(Token::CloseBrace) {
                    return Err(());
                } else {
                    break;
                }
            }
        }
        
        Ok(fields)
    }

    fn union_case(&mut self) -> ParseResult<Node<UnionCase<RawSymbol>>> {
        let name = match self.eat_unqualified_name() {
            Some(Spanned { value, span }) => Node::new(value, span),
            None => return Err(()),
        };
        let mut args = Vec::new();
        while let Some(result) = self.type_term() {
            args.push(try!(result));
        }

        let span = if args.is_empty() {
            name.span.clone()
        } else {
            name.span.merge(&args[args.len() - 1].span)
        };

        Ok(Node::new(UnionCase {
            tag: name,
            args: args,
        }, span))
    }

    fn trait_(&mut self) -> ParseResult<Node<Trait<RawSymbol>>> {
        let span = self.previous_span();

        let name = match self.eat_unqualified_name() {
            Some(Spanned { value, span }) => Node::new(value, span),
            None => return Err(()),
        };

        let mut vars = Vec::new();
        while let Some(Spanned { value, span }) = self.eat_var_name() {
            vars.push(Node::new(value, span));
        }

        let mut base_traits = Vec::new();
        if self.eat(Token::Colon) {
            loop {
                base_traits.push(try!(self.type_()));
                if !self.eat(Token::Comma) {
                    break;
                }
            }
        }

        try!(self.expect(Token::Where));

        let old_indent = self.align_on_next();

        let mut values = Vec::new();
        loop {
            self.accept_aligned = true;
            match self.type_annot() {
                Some(Ok(annot)) => values.push(annot),
                Some(Err(_)) => {
                    if !self.skip_to_aligned() {
                        break;
                    }
                }
                None => break,
            }
        }

        self.accept_aligned = false;
        self.current_indent = old_indent;

        let span = span.merge(&self.previous_span());
        Ok(Node::new(Trait {
            name: name,
            vars: vars,
            base_traits: base_traits,
            values: values,
        }, span))
    }

    fn type_annot(&mut self) -> Option<ParseResult<Node<TypeAnnot<RawSymbol>>>> {
        let symbol = if self.eat(Token::OpenParen) {
            let span = self.previous_span();
            let name = match self.eat_operator() {
                Some(Spanned { value, .. }) => value,
                None => return Some(Err(())),
            };
            if !self.eat(Token::CloseParen) {
                return Some(Err(()));
            }
            let span = span.merge(&self.previous_span());
            Node::new(name, span)
        } else {
            match self.eat_unqualified_name() {
                Some(Spanned { value, span }) => Node::new(value, span),
                None => {
                    return if self.peek().is_none() {
                        None
                    } else {
                        Some(Err(()))
                    };
                }
            }
        };

        if !self.eat(Token::Comma) {
            return Some(Err(()));
        }

        let type_ = match self.scheme() {
            Ok(scheme) => scheme,
            Err(()) => return Some(Err(())),
        };
        let span = symbol.span.merge(&type_.span);

        Some(Ok(Node::new(TypeAnnot {
            value: symbol,
            type_: type_,
        }, span)))
    }

    fn impl_(&mut self) -> ParseResult<Node<Impl<RawSymbol>>> {
        let span = self.previous_span();
        let scheme = try!(self.scheme());
        try!(self.expect(Token::Colon));
        let trait_ = try!(self.type_());
        try!(self.expect(Token::Where));
        let mut values = Vec::new();
        let old_indent = self.align_on_next();

        loop {
            self.accept_aligned = true;
            if self.peek().is_none() {
                break;
            }

            match self.let_decl(false) {
                Ok(Node { node, span, .. }) => {
                    match node {
                        LetDecl::Def(def) => values.push(Node::new(def, span)),
                        LetDecl::Type(_) => unreachable!(),
                    }
                }
                Err(()) => {
                    break;
                }
            }
        }

        self.accept_aligned = true;
        self.current_indent = old_indent;

        let span = span.merge(&self.previous_span());
        
        Ok(Node::new(Impl {
            scheme: scheme,
            trait_: trait_,
            values: values,
        }, span))
    }

    fn decl(&mut self) -> ParseResult<Node<Decl<RawSymbol>>> {
        if self.eat(Token::Type) {
            self.type_decl()
        } else if self.eat(Token::Trait) {
            self.trait_().map(|n| n.map(Decl::Trait))
        } else if self.eat(Token::Impl) {
            self.impl_().map(|n| n.map(Decl::Impl))
        } else {
            self.let_decl(true).map(|n| n.map(Decl::Let))
        }
    }

    fn exposed_item_list<S, F>(&mut self, eat_symbol: F) -> ParseResult<Vec<Node<ListItem<ExposedItem<S>>>>>
            where F: Fn(&mut Self) -> Option<Spanned<S>> {
        let mut items = Vec::new();

        loop {
            if self.eat(Token::DotDot) {
                let span = self.previous_span();
                items.push(Node::new(ListItem::All, span))
            } else {
                let name = match eat_symbol(self) {
                    Some(Spanned { value, span }) => Node::new(value, span),
                    None => {
                        self.emit_error();
                        return Err(());
                    }
                };

                let subitems = if self.eat(Token::OpenParen) {
                    try!(self.exposed_subitem_list())
                } else {
                    Vec::new()
                };

                let alias = if self.eat(Token::As) {
                    match self.eat_unqualified_name() {
                        Some(Spanned { value, span }) => Some(Node::new(value, span)),
                        None => {
                            self.emit_error();
                            return Err(());
                        }
                    }
                } else {
                    None
                };

                let span = name.span.merge(&self.previous_span());

                let item = ExposedItem {
                    name: name,
                    subitems: subitems,
                    alias: alias,
                };

                items.push(Node::new(ListItem::Concrete(item), span));
            }

            if !self.eat(Token::Comma) {
                if self.eat(Token::CloseParen) {
                    break;
                } else {
                    self.emit_error();
                    return Err(());
                }
            }
            if self.eat(Token::CloseParen) {
                break;
            }
        }

        Ok(items)
    }

    fn exposed_subitem_list(&mut self) -> ParseResult<Vec<Node<ListItem<ExposedSubitem>>>> {
        let mut subitems = Vec::new();
        
        loop {
            if self.eat(Token::DotDot) {
                let span = self.previous_span();
                subitems.push(Node::new(ListItem::All, span));
            } else {
                let name = match self.eat_unqualified_name() {
                    Some(Spanned { value, span }) => Node::new(value, span),
                    None => {
                        self.emit_error();
                        return Err(());
                    }
                };

                let item_span;
                let alias = if self.eat(Token::As) {
                    match self.eat_unqualified_name() {
                        Some(Spanned { value, span }) => {
                            item_span = name.span.merge(&span);
                            Some(Node::new(value, span))
                        }
                        None => {
                            self.emit_error();
                            return Err(());
                        }
                    }
                } else {
                    item_span = name.span.clone();
                    None
                };

                subitems.push(Node::new(ListItem::Concrete(ExposedSubitem {
                    name: name,
                    alias: alias,
                }), item_span));
            }

            if !self.eat(Token::Comma) {
                if self.eat(Token::CloseParen) {
                    break;
                } else {
                    self.emit_error();
                    return Err(());
                }
            }
            if self.eat(Token::CloseParen) {
                break;
            }
        }

        Ok(subitems)
    }

    fn module_def(&mut self) -> ParseResult<Node<ModuleDef>> {
        try!(self.expect(Token::Module));
        let span_start = self.previous_span();

        let name = match self.eat_symbol() {
            Some(Spanned { value, span }) => Node::new(value.full_name(), span),
            None => {
                self.emit_error();
                return Err(());
            }
        };

        try!(self.expect(Token::Exposing));
        try!(self.expect(Token::OpenParen));

        let items = try!(self.exposed_item_list(Parser::eat_symbol));
        let span = span_start.merge(&self.previous_span());

        Ok(Node::new(ModuleDef {
            name: name,
            exposing: items,
        }, span))
    }

    fn import(&mut self) -> ParseResult<Node<Import>> {
        let span_start = self.previous_span();

        let name = match self.eat_symbol() {
            Some(Spanned { value, span }) => Node::new(value.full_name(), span),
            None => {
                self.emit_error();
                return Err(());
            }
        };

        let alias = if self.eat(Token::As) {
            match self.eat_unqualified_name() {
                Some(Spanned { value, span }) => Some(Node::new(value, span)),
                None => {
                    self.emit_error();
                    return Err(());
                }
            }
        } else {
            None
        };

        let exposing = if self.eat(Token::Exposing) {
            try!(self.exposed_item_list(Parser::eat_unqualified_name))
        } else {
            Vec::new()
        };

        let span = span_start.merge(&self.previous_span());

        Ok(Node::new(Import {
            name: name,
            alias: alias,
            exposing: exposing,
        }, span))
    }

    fn module(&mut self, require_def: bool) -> ParseResult<Module<RawSymbol>> {
        self.accept_aligned = true;
        self.current_indent = 1;

        let def = if require_def {
            match self.module_def() {
                Ok(def) => def,
                Err(()) => return Err(()),
            }
        } else {
            let span = Span::new(&Position::new(1, 1), &Position::new(1, 1));
            let def = ModuleDef {
                name: Node::new("<main>".to_string(), span.clone()),
                exposing: Vec::new(),
            };
            Node::new(def, span)
        };

        let mut imports = Vec::new();
        let mut items = Vec::new();

        while !self.is_at_end() {
            self.accept_aligned = true;
            if self.eat(Token::Import) {
                match self.import() {
                    Ok(import) => { imports.push(import); }
                    Err(()) => { self.skip_to_aligned(); }
                }
            } else {
                match self.decl() {
                    Ok(decl) => { items.push(decl); }
                    Err(()) => { self.skip_to_aligned(); }
                }
            }
        }

        let module = Module {
            def: def,
            imports: imports,
            items: items,
        };

        Ok(module)
    }
}

fn operator_right_section(operator: Node<String>, expr: ExprNode, span: Span) -> ExprNode {
    let pattern = Pattern::Var("#".to_string());
    let pattern_node = Node::new(pattern, operator.span.clone());
    let value = Expr::Ident(RawSymbol::Unqualified("#".to_string()));
    let value_node = Node::new(value, operator.span.clone());
    let operator = Node::new(RawSymbol::Unqualified(operator.node), operator.span);
    let body = Expr::Infix(Box::new(value_node), operator, Box::new(expr));
    let body_node = Node::new(body, span.clone());
    let lambda = Expr::Lambda(vec![pattern_node], Box::new(body_node));
    let lambda_node = Node::new(lambda, span);
    lambda_node
}

fn operator_left_section(operator: Node<String>, expr: ExprNode, span: Span) -> ExprNode {
    let pattern = Pattern::Var("#".to_string());
    let pattern_node = Node::new(pattern, operator.span.clone());
    let value = Expr::Ident(RawSymbol::Unqualified("#".to_string()));
    let value_node = Node::new(value, operator.span.clone());
    let operator = Node::new(RawSymbol::Unqualified(operator.node), operator.span);
    let body = Expr::Infix(Box::new(expr), operator, Box::new(value_node));
    let body_node = Node::new(body, span.clone());
    let lambda = Expr::Lambda(vec![pattern_node], Box::new(body_node));
    let lambda_node = Node::new(lambda, span);
    lambda_node
}

#[cfg(test)]
mod tests {
    use parsing::lexer::lex;
    use ast::{Expr, Pattern, Literal, Type, Scheme, LetDecl, Def, TypeAnnot, RawSymbol};
    use super::Parser;

    fn write_symbol(symbol: &RawSymbol, output: &mut String) {
        match *symbol {
            RawSymbol::Qualified(ref path, ref name) => {
                output.push_str(path);
                output.push_str(".");
                output.push_str(name);
            }
            RawSymbol::Unqualified(ref name) => {
                output.push_str(name);
            }
            RawSymbol::Trusted(ref path, ref name) => {
                output.push_str("#");
                output.push_str(path);
                output.push_str(".");
                output.push_str(name);
            }
        }
    }

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

    fn write_expr(expr: &Expr<RawSymbol>, output: &mut String) {
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
            Expr::Ident(ref symbol) => {
                write_symbol(symbol, output);
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
                write_symbol(&op.node, output);
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
            Expr::Lambda(ref params, ref value) => {
                output.push_str("(lambda (");
                let mut need_space = false;
                for param in params {
                    if need_space {
                        output.push(' ');
                    }
                    write_pattern(&param.node, output);
                    need_space = true;
                }
                output.push_str(") ");
                write_expr(&value.node, output);
                output.push_str(")");
            }
            Expr::Case(ref value, ref branches) => {
                output.push_str("(case ");
                write_expr(&value.node, output);
                for branch in branches {
                    output.push_str(" (");
                    write_pattern(&branch.node.pattern.node, output);
                    match branch.node.guard {
                        Some(ref guard) => {
                            output.push(' ');
                            write_expr(&guard.node, output);
                        }
                        None => { }
                    }
                    output.push(' ');
                    write_expr(&branch.node.value.node, output);
                    output.push(')');

                }
                output.push(')');
            }
            Expr::Let(ref decls, ref value) => {
                output.push_str("(let ");
                for decl in decls {
                    write_let_decl(&decl.node, output);
                    output.push(' ');
                }
                write_expr(&value.node, output);
                output.push(')');
            }
        }
    }

    fn write_pattern(pattern: &Pattern<RawSymbol>, output: &mut String) {
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
                write_symbol(&tag.node, output);
                for arg in args {
                    output.push(' ');
                    write_pattern(&arg.node, output);
                }
                output.push(')');
            }
            Pattern::Infix(ref lhs, ref op, ref rhs) => {
                output.push_str("(opdec ");
                write_symbol(&op.node, output);
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

    fn write_let_decl(decl: &LetDecl<RawSymbol>, output: &mut String) {
        match *decl {
            LetDecl::Def(Def { ref pattern, ref value }) => {
                output.push_str("(def ");
                write_pattern(&pattern.node, output);
                output.push(' ');
                write_expr(&value.as_ref().unwrap().node, output);
                output.push(')');
            }
            LetDecl::Type(TypeAnnot { ref value, ref type_ }) => {
                output.push_str("(typeannot ");
                output.push_str(&value.node);
                output.push(' ');
                write_scheme(&type_.node, output);
                output.push(')');
            }
        }
    }

    fn write_scheme(scheme: &Scheme<RawSymbol>, output: &mut String) {
        if scheme.vars.is_empty() {
            write_type(&scheme.type_.node, output);
        } else {
            unimplemented!()
        }
    }

    fn write_type(type_: &Type<RawSymbol>, output: &mut String) {
        match *type_ {
            Type::SelfType => {
                output.push_str("self");
            }
            Type::Var(ref var) => {
                output.push_str("(var ");
                output.push_str(var);
                output.push_str(")");
            }
            Type::Apply(ref a, ref b) => {
                output.push_str("(apply ");
                write_type(&a.node, output);
                output.push(' ');
                write_type(&b.node, output);
                output.push(')');
            }
            Type::Concrete(ref t) => {
                write_symbol(&t, output);
            }
            Type::Function(ref a, ref b) => {
                output.push_str("(fn ");
                write_type(&a.node, output);
                output.push(' ');
                write_type(&b.node, output);
                output.push(')');
            }
        }
    }

    fn check_expr(source: &str, expected: &str) {
        let (tokens, errors) = lex(source);
        assert!(errors.is_empty());
        let mut parser = Parser::new(tokens.into_iter());
        let expr = parser.expr(false).ok().unwrap();
        let mut printed = String::new();
        write_expr(&expr.node, &mut printed);
        assert_eq!(expected, printed);
        assert!(parser.errors.is_empty());
    }

    fn check_pattern(source: &str, expected: &str) {
        let (tokens, errors) = lex(source);
        assert!(errors.is_empty());
        let mut parser = Parser::new(tokens.into_iter());
        let pattern = parser.pattern().ok().unwrap();
        let mut printed = String::new();
        write_pattern(&pattern.node, &mut printed);
        assert_eq!(expected, printed);
        assert!(parser.errors.is_empty());
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
        check_pattern("(a)", "(parens (var a))");
    }
    
    #[test]
    fn alias_pattern() {
        check_pattern(
            "Some x as opt",
            "(alias opt (dec Some (var x)))");
    }

    #[test]
    fn lambda() {
        check_expr(
            r#" \a b -> a "#,
            "(lambda ((var a) (var b)) a)");
    }

    #[test]
    fn lambda2() {
        check_expr(
            r#" \(Wrapped a) _ -> \_ -> a "#,
            "(lambda ((parens (dec Wrapped (var a))) _) (lambda (_) a))");
    }

    #[test]
    fn case() {
        check_expr(
            "
case n of
    1 -> true
    Some _ if false -> false
    _ -> true",
            "(case n (1 true) ((dec Some _) false false) (_ true))");
    }
    
    #[test]
    fn nested_case() {
        check_expr(
            "
case n of
    A -> true
    B a -> case a of
        A -> 1
        B _ -> 2
        C -> 3
    C -> 4",
            "(case n ((dec A) true) ((dec B (var a)) (case a ((dec A) 1) ((dec B _) 2) ((dec C) 3))) ((dec C) 4))");
    }

    #[test]
    fn basic_let() {
        check_expr(
            "let a = 1 in a",
            "(let (def (var a) 1) a)");
    }
    
    #[test]
    fn basic_let_2() {
        check_expr(
            "
let
    x = 1
    a + b = 2
    x : Int
in
    x + x",
            "(let (def (var x) 1) (def (var +) (lambda ((var a) (var b)) 2)) (typeannot x Int) (+ x x))");
    }

    #[test]
    fn nested_let() {
        check_expr(
            "
let
    a + b = 3
    (Wrapped x) = wrapped
          in let
       f _ = 3 + x
     in f",
            "(let (def (var +) (lambda ((var a) (var b)) 3)) (def (parens (dec Wrapped (var x))) wrapped) (let (def (var f) (lambda (_) (+ 3 x))) f))");
    }
    
    #[test]
    fn basic_do() {
        check_expr(
            "
do  a <- value
    foo",
            "(apply #Core.Monad.bind value (lambda ((var a)) foo))");
    }
    
    #[test]
    fn do_if() {
        check_expr(
            "
do  a <- value
    let b = a
    if b
    c",
            "(apply #Core.Monad.bind value (lambda ((var a)) (let (def (var b) a) (if b c #Core.Monad.empty))))");
    }
}
