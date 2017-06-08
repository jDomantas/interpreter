use std::cmp::Ordering;
use std::iter::Peekable;
use position::{Span, Spanned, DUMMY_SPAN};
use parsing::tokens::Token;
use ast::{Expr, Literal, Pattern, CaseBranch, Node, LetDecl, Def, TypeAnnot, Scheme, Type, RawSymbol};


pub fn parse<I: Iterator<Item=Spanned<Token>>>(_tokens: I) -> ! {
    // let mut parser = Parser::new(tokens);
    unimplemented!()
}

#[derive(PartialEq, Debug, Clone)]
enum TokenKind {
    Token(Token),
    Ident,
    UnqualifiedName,
    VarName,
    Literal,
    Operator,
}

type ExprNode = Node<Expr<RawSymbol>>;
type PatternNode = Node<Pattern<RawSymbol>>;
type TypeNode = Node<Type<RawSymbol>>;

fn error_expr_node() -> ExprNode {
    Node::new(Expr::Error, DUMMY_SPAN.clone())
}

fn error_pattern_node() -> PatternNode {
    Node::new(Pattern::Error, DUMMY_SPAN.clone())
}

type ParseResult<T> = Result<T, T>;

enum MaybeParsed<T> {
    Ok(T),
    Err(T),
    Empty,
}

impl<T> MaybeParsed<T> {
    fn from_result(result: ParseResult<T>) -> MaybeParsed<T> {
        match result {
            Ok(x) => MaybeParsed::Ok(x),
            Err(x) => MaybeParsed::Err(x),
        }
    }

    fn map<U, F: FnOnce(T) -> U>(self, f: F) -> MaybeParsed<U> {
        match self {
            MaybeParsed::Ok(x) => MaybeParsed::Ok(f(x)),
            MaybeParsed::Err(x) => MaybeParsed::Err(f(x)),
            MaybeParsed::Empty => MaybeParsed::Empty,
        }
    }

    fn is_ok(&self) -> bool {
        match *self {
            MaybeParsed::Ok(_) => true,
            _ => false,
        }
    }
}

enum MaybeParsedType {
    Ok(Node<Type<RawSymbol>>),
    Err,
    Empty,
}

struct ParseError;

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
        unimplemented!();
    }

    fn previous_span(&self) -> Span {
        self.last_token_span.clone()
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
    
    fn expr_term(&mut self) -> MaybeParsed<ExprNode> {
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
                        Node::new(Expr::Ident(RawSymbol::Unqualified(operator.value)), self.previous_span())
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
                        Ok(expr) => expr,
                        Err(expr) => return MaybeParsed::Err(expr),
                    }
                }
            };

            if self.eat(Token::CloseParen) {
                let node_span = start_span.merge(&self.previous_span());
                let expr = Expr::Parenthesised(Box::new(expr));
                return MaybeParsed::Ok(Node::new(expr, node_span));
            } else if self.eat(Token::Comma) {
                // TODO: parse tuple
                unimplemented!()
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

    fn application(&mut self) -> ParseResult<ExprNode> {
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

    fn expr_operation(&mut self, mut can_section: bool) -> ParseResult<ExprNode> {
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
                    let op = Node::new(RawSymbol::Unqualified(operator.value), operator.span);
                    let lhs = Box::new(result);
                    result = Node::new(Expr::Infix(lhs, op, rhs), span);
                }
                Err(rhs) => {
                    let span = result.span.merge(&rhs.span);
                    let rhs = Box::new(rhs);
                    let op = Node::new(RawSymbol::Unqualified(operator.value), operator.span);
                    let lhs = Box::new(result);
                    return Err(Node::new(Expr::Infix(lhs, op, rhs), span));
                }
            }
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
            // TODO: parse do
            unimplemented!()
        } else {
            self.expr_operation(can_section)
        }
    }

    fn pattern_term(&mut self) -> MaybeParsed<PatternNode> {
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
                Ok(pattern) => pattern,
                Err(pattern) => return MaybeParsed::Err(pattern),
            };
            
            if self.eat(Token::CloseParen) {
                let node_span = start_span.merge(&self.previous_span());
                let pattern = Pattern::Parenthesised(Box::new(pattern));
                return MaybeParsed::Ok(Node::new(pattern, node_span));
            } else if self.eat(Token::Comma) {
                // TODO: parse tuple pattern
                unimplemented!()
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

    fn deconstruct(&mut self) -> ParseResult<PatternNode> {
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
                match self.eat_unqualified_name() {
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

    fn pattern(&mut self) -> ParseResult<PatternNode> {
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
                    let op = Node::new(RawSymbol::Unqualified(operator.value), operator.span);
                    let lhs = Box::new(result);
                    result = Node::new(Pattern::Infix(lhs, op, rhs), span);
                }
                Err(rhs) => {
                    let span = result.span.merge(&rhs.span);
                    let rhs = Box::new(rhs);
                    let op = Node::new(RawSymbol::Unqualified(operator.value), operator.span);
                    let lhs = Box::new(result);
                    return Err(Node::new(Pattern::Infix(lhs, op, rhs), span));
                }
            }
        }
    }
    
    fn if_(&mut self) -> ParseResult<ExprNode> {
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

    fn lambda(&mut self) -> ParseResult<ExprNode> {
        let start_span = self.previous_span();

        let mut params = Vec::new();
        loop {
            match self.pattern_term() {
                MaybeParsed::Ok(pattern) => params.push(pattern),
                MaybeParsed::Err(_) => return Err(error_expr_node()),
                MaybeParsed::Empty => {
                    if params.is_empty() {
                        self.emit_error();
                        return Err(error_expr_node());
                    } else {
                        break;
                    }
                }
            }
        }

        if !self.eat(Token::Arrow) {
            self.emit_error();
            return Err(error_expr_node());
        }

        match self.expr(false) {
            Ok(expr) => {
                let span = start_span.merge(&expr.span);
                Ok(Node::new(Expr::Lambda(params, Box::new(expr)), span))
            }
            Err(expr) => {
                let span = start_span.merge(&expr.span);
                Err(Node::new(Expr::Lambda(params, Box::new(expr)), span))
            }
        }
    }

    fn case(&mut self) -> ParseResult<ExprNode> {
        let value = match self.expr(false) {
            Ok(expr) => expr,
            Err(_) => return Err(error_expr_node()),
        };

        if !self.eat(Token::Of) {
            self.emit_error();
            return Err(error_expr_node());
        }

        let old_indent = self.align_on_next();
        let mut branches = Vec::new();
        let mut span = self.previous_span();

        loop {
            self.accept_aligned = true;

            if self.peek().is_none() {
                if branches.is_empty() {
                    // do this to fill wanted token list
                    debug_assert!(self.case_branch().is_err());
                    self.emit_error();
                    self.current_indent = old_indent;
                    self.accept_aligned = false;
                    return Err(error_expr_node());
                } else {
                    break;
                }
            }

            match self.case_branch() {
                Ok(branch) => {
                    span = span.merge(&branch.span);
                    branches.push(branch);
                }
                Err(branch) => {
                    span = span.merge(&branch.span);
                    branches.push(branch);
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
        
        let pattern = match self.pattern() {
            Ok(pattern) => pattern,
            Err(pattern) => {
                let span = pattern.span.clone();
                return Err(Node::new(CaseBranch {
                    pattern: pattern,
                    value: error_expr_node(),
                    guard: None,
                }, span));
            }
        };

        let guard = if self.eat(Token::If) {
            match self.expr(false) {
                Ok(expr) => Some(expr),
                Err(expr) => {
                    let span = pattern.span.merge(&expr.span);
                    return Err(Node::new(CaseBranch {
                        pattern: pattern,
                        value: error_expr_node(),
                        guard: Some(expr),
                    }, span));
                }
            }
        } else {
            None
        };

        if !self.eat(Token::Arrow) {
            let span = pattern.span.clone();
            return Err(Node::new(CaseBranch {
                pattern: pattern,
                value: error_expr_node(),
                guard: None,
            }, span));
        }

        match self.expr(false) {
            Ok(expr) => {
                let span = pattern.span.merge(&expr.span);
                Ok(Node::new(CaseBranch {
                    pattern: pattern,
                    value: expr,
                    guard: guard,
                }, span))
            }
            Err(expr) => {
                let span = pattern.span.merge(&expr.span);
                Err(Node::new(CaseBranch {
                    pattern: pattern,
                    value: expr,
                    guard: guard,
                }, span))
            }
        }
    }

    fn let_(&mut self) -> ParseResult<ExprNode> {
        let old_indent = self.align_on_next();
        let mut decls = Vec::new();
        let mut span = self.previous_span();
        let mut ate_in = false;

        loop {
            self.accept_aligned = true;

            if self.peek().is_none() || { ate_in = self.eat(Token::In); ate_in } {
                if decls.is_empty() {
                    // do this to fill wanted token list
                    debug_assert!(!self.let_decl().is_ok());
                    self.emit_error();
                    self.current_indent = old_indent;
                    self.accept_aligned = false;
                    return Err(error_expr_node());
                } else {
                    break;
                }
            }

            match self.let_decl() {
                MaybeParsed::Ok(decl) => {
                    span = span.merge(&decl.span);
                    decls.push(decl);
                }
                MaybeParsed::Err(decl) => {
                    span = span.merge(&decl.span);
                    decls.push(decl);
                    if !self.skip_to_aligned() {
                        break;
                    }
                }
                MaybeParsed::Empty => {
                    if !self.skip_to_aligned() {
                        break;
                    }
                }
            }
        }

        self.current_indent = old_indent;
        self.accept_aligned = false;

        if !ate_in && !self.eat(Token::In) {
            self.emit_error();
            return Err(error_expr_node());
        }

        match self.expr(false) {
            Ok(expr) => {
                span = span.merge(&expr.span);
                Ok(Node::new(Expr::Let(decls, Box::new(expr)), span))
            }
            Err(expr) => {
                span = span.merge(&expr.span);
                Err(Node::new(Expr::Let(decls, Box::new(expr)), span))
            }
        }
    }

    fn let_decl(&mut self) -> MaybeParsed<Node<LetDecl<RawSymbol>>> {
        let is_paren = match self.peek() {
            Some(&Spanned { value: Token::OpenParen, .. }) => true,
            _ => false,
        };
        let is_op = match self.peek2() {
            Some(&Spanned { value: Token::Operator(_), .. }) => true,
            _ => false,
        };

        if is_paren && is_op {
            debug_assert!(self.eat(Token::OpenParen));
            let symbol = match self.consume() {
                Some(Spanned { value: Token::Operator(op), span }) => {
                    Node::new(op, span)
                }
                _ => {
                    panic!("expected to get operator");
                }
            };

            if self.eat(Token::CloseParen) {
                self.type_or_def(symbol)
            } else {
                self.emit_error();
                MaybeParsed::Empty
            }
        } else if is_paren || is_op {
            self.pattern_assign().map(|n| n.map(LetDecl::Def))
        } else {
            match self.eat_var_name() {
                Some(Spanned { value, span }) => {
                    self.type_or_def(Node::new(value, span))
                }
                None => {
                    debug_assert!(!self.eat(Token::OpenParen));
                    self.emit_error();
                    MaybeParsed::Empty
                }
            }
        }
    }

    fn scheme(&mut self) -> Option<Node<Scheme<RawSymbol>>> {
        if self.eat(Token::OpenParen) {
            let mut constraints = Vec::new();
            loop {
                let var = match self.eat_var_name() {
                    Some(Spanned { value, span }) => Node::new(value, span),
                    None => return None,
                };
                if !self.eat(Token::Colon) {
                    self.emit_error();
                    return None;
                }
                let type_ = match self.type_() {
                    Some(type_) => type_,
                    None => return None,
                };
                if self.eat(Token::CloseParen) {
                    break;
                } else if !self.eat(Token::Comma) {
                    self.emit_error();
                    return None;
                }
                constraints.push((var, type_));
            }
            if !self.eat(Token::FatArrow) {
                self.emit_error();
                return None;
            }
            match self.type_() {
                Some(type_) => {
                    let span = constraints[0].0.span.merge(&type_.span);
                    let scheme = Scheme {
                        vars: constraints,
                        type_: type_,
                    };
                    Some(Node::new(scheme, span))
                }
                None => None,
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

    fn type_term(&mut self) -> MaybeParsedType {
        match self.eat_symbol() {
            Some(Spanned { value, span }) => {
                return MaybeParsedType::Ok(Node::new(Type::from_symbol(value), span));
            }
            _ => { }
        }

        if self.eat(Token::OpenParen) {
            let type_ = match self.type_() {
                Some(type_) => type_,
                None => return MaybeParsedType::Err,
            };

            if self.eat(Token::Comma) {
                // TODO: parse tuple type
                unimplemented!()
            } else if self.eat(Token::CloseParen) {
                MaybeParsedType::Ok(type_)
            } else {
                self.emit_error();
                MaybeParsedType::Err
            }
        } else {
            MaybeParsedType::Empty
        }
    }

    fn type_application(&mut self) -> Option<TypeNode> {
        let mut result = match self.type_term() {
            MaybeParsedType::Ok(type_) => type_,
            MaybeParsedType::Err |
            MaybeParsedType::Empty => return None,
        };

        loop {
            match self.type_term() {
                MaybeParsedType::Ok(type_) => {
                    let span = result.span.merge(&type_.span);
                    result = Node::new(Type::Apply(Box::new(result), Box::new(type_)), span);
                }
                MaybeParsedType::Err => {
                    return None;
                }
                MaybeParsedType::Empty => {
                    return Some(result);
                }
            }
        }
    }

    fn type_(&mut self) -> Option<TypeNode> {
        let arg = match self.type_application() {
            Some(type_) => type_,
            None => return None,
        };

        if self.eat(Token::Arrow) {
            match self.type_() {
                Some(result) => {
                    let span = arg.span.merge(&result.span);
                    Some(Node::new(Type::Function(Box::new(arg), Box::new(result)), span))
                }
                None => {
                    None
                }
            }
        } else {
            Some(arg)
        }
    }

    fn pattern_assign(&mut self) -> MaybeParsed<Node<Def<RawSymbol>>> {
        let first = match self.pattern_term() {
            MaybeParsed::Ok(pattern) => pattern,
            MaybeParsed::Err(_) => return MaybeParsed::Empty,
            MaybeParsed::Empty => return MaybeParsed::Empty,
        };

        if self.eat(Token::Equals) {
            match self.expr(false) {
                Ok(expr) => {
                    let span = first.span.merge(&expr.span);
                    MaybeParsed::Ok(Node::new(Def {
                        pattern: first,
                        value: expr,
                    }, span))
                }
                Err(expr) => {
                    let span = first.span.merge(&expr.span);
                    MaybeParsed::Err(Node::new(Def {
                        pattern: first,
                        value: expr,
                    }, span))
                }
            }
        } else {
            let (op, second) = match self.eat_operator() {
                Some(Spanned { value, span }) => {
                    match self.pattern_term() {
                        MaybeParsed::Ok(pattern) => (Node::new(value, span), pattern),
                        MaybeParsed::Err(_) => return MaybeParsed::Empty,
                        MaybeParsed::Empty => return MaybeParsed::Empty,
                    }
                }
                None => return MaybeParsed::Empty,
            };

            if !self.eat(Token::Equals) {
                self.emit_error();
                return MaybeParsed::Empty;
            }

            let pattern = op.map(Pattern::Var);
            let params = vec![first, second];

            let (expr, is_ok) = match self.expr(false) {
                Ok(expr) => (expr, true),
                Err(expr) => (expr, false),
            };

            let lambda_span = params[0].span.merge(&expr.span);
            let expr = Node::new(Expr::Lambda(params, Box::new(expr)), lambda_span);
            let span = pattern.span.merge(&expr.span);

            let def = Def {
                pattern: pattern,
                value: expr,
            };

            let node = Node::new(def, span);

            if is_ok {
                MaybeParsed::Ok(node)
            } else {
                MaybeParsed::Err(node)
            }
        }
    }

    fn type_or_def(&mut self, symbol: Node<String>) -> MaybeParsed<Node<LetDecl<RawSymbol>>> {
        if self.eat(Token::Colon) {
            match self.scheme() {
                Some(scheme) => {
                    let node_span = symbol.span.merge(&scheme.span);
                    MaybeParsed::Ok(Node::new(LetDecl::Type(TypeAnnot {
                        value: symbol,
                        type_: scheme,
                    }), node_span))
                }
                None => {
                    MaybeParsed::Empty
                }
            }
        } else {
            let mut params = Vec::new();
            let pattern = symbol.map(Pattern::Var);
            loop {
                match self.pattern_term() {
                    MaybeParsed::Ok(pattern) => params.push(pattern),
                    MaybeParsed::Err(_) => {
                        let span = pattern.span.clone();
                        let def = LetDecl::Def(Def {
                            pattern: pattern,
                            value: error_expr_node(),
                        });
                        return MaybeParsed::Err(Node::new(def, span));
                    }
                    MaybeParsed::Empty => {
                        if self.eat(Token::Equals) {
                            break;
                        } else {
                            let span = pattern.span.clone();
                            let def = LetDecl::Def(Def {
                                pattern: pattern,
                                value: error_expr_node(),
                            });
                            return MaybeParsed::Err(Node::new(def, span));
                        }
                    }
                }
            }

            let (expr, is_ok) = match self.expr(false) {
                Ok(expr) => (expr, true),
                Err(expr) => (expr, false),
            };

            let expr = if !params.is_empty() {
                let span = params[0].span.merge(&expr.span);
                Node::new(Expr::Lambda(params, Box::new(expr)), span)
            } else {
                expr
            };

            let span = pattern.span.merge(&expr.span);
            let node = Node::new(LetDecl::Def(Def {
                pattern: pattern,
                value: expr,
            }), span);

            if is_ok {
                MaybeParsed::Ok(node)
            } else {
                MaybeParsed::Err(node)
            }
        }
    }
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
            Expr::Error => {
                panic!("found error node");
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
            Pattern::Error => {
                panic!("found error pattern node");
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
                write_expr(&value.node, output);
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
}
