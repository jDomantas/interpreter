use std::cmp::Ordering;
use std::iter::Peekable;
use errors::{Error, parse_error};
use position::{Position, Span, DUMMY_SPAN};
use parsing::tokens::{Token, TokenKind};
use ast::{Node, Literal, Associativity};
use ast::parsed::{
    Expr, Pattern, CaseBranch, LetDecl, Decl, Def, TypeAnnot, Scheme, Type,
    TypeAlias, UnionType, UnionCase, RecordType, Symbol, Trait, Impl, ItemList,
    ExposedItem, ModuleDef, Import, Module, DoExpr,
};


pub fn parse_module<I>(tokens: I, module: &str, require_def: bool) -> (Option<Module>, Vec<Error>)
        where I: Iterator<Item=Node<Token>> {
    let mut parser = Parser::new(tokens, module);
    let module = parser.module(require_def).ok();
    debug_assert!(parser.is_at_end());

    (module, parser.errors)
}

type ExprNode = Node<Expr>;
type PatternNode = Node<Pattern>;
type TypeNode = Node<Type>;

type ParseResult<T> = Result<T, ()>;

struct Parser<'a, I: Iterator<Item=Node<Token>>> {
    next_token: Option<Node<Token>>,
    tokens: Peekable<I>,
    expected_tokens: Vec<TokenKind>,
    errors: Vec<Error>,
    current_indent: usize,
    accept_aligned: bool,
    last_token_span: Span,
    module: &'a str,
}

impl<'a, I: Iterator<Item=Node<Token>>> Parser<'a, I> {
    fn new(mut tokens: I, module: &'a str) -> Parser<'a, I> {
        let next_token = tokens.next();
        Parser {
            next_token: next_token,
            tokens: tokens.peekable(),
            expected_tokens: Vec::new(),
            errors: Vec::new(),
            current_indent: 0,
            accept_aligned: false,
            last_token_span: DUMMY_SPAN,
            module: module,
        }
    }

    fn emit_error(&mut self) {
        let (token, span) = match self.next_token {
            // don't report errors on error tokens,
            // these will be reported by lexer
            Some(Node { value: Token::Error, .. }) => return,
            Some(Node { ref value, span }) => (value, span),
            _ => panic!("skipped EndOfInput token"),
        };

        self.expected_tokens.sort_by_key(TokenKind::to_string);
        self.expected_tokens.dedup();

        let any_matches = self.expected_tokens.iter().any(|kind| kind.matches_token(token));

        let mut message = if self.expected_tokens.is_empty() {
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
                message.push_str(token.to_string());
                add_comma = true;
            }
            message
        };

        message.push('.');

        if any_matches {
            message.push_str(" Make sure that your code is properly indented.");
        }

        self.errors.push(parse_error(message, span, self.module));
    }

    fn bad_module_name(&mut self) {
        let span = self.previous_span();
        let message = format!("Expected to find module `{}`", self.module);
        self.errors.push(parse_error(message, span, self.module));
    }

    fn previous_span(&self) -> Span {
        self.last_token_span
    }

    fn next_span(&self) -> Span {
        match self.next_token {
            Some(Node { span, .. }) => span,
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
        match self.next_token {
            Some(Node { value: Token::EndOfInput, .. }) => {
                return false;
            }
            _ => { }
        }
        // skip at least one to make sure we do not get stuck on aligned error
        self.consume();
        loop {
            match self.next_token {
                Some(Node { value: Token::EndOfInput, .. }) => {
                    return false;
                }
                Some(Node { ref span, .. }) => {
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

    fn peek(&self) -> Option<&Node<Token>> {
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

    fn peek2(&mut self) -> Option<&Node<Token>> {
        // next token might be too much to the left or missing altogehter
        // so first we have to make sure it is there
        if self.peek().is_none() {
            return None;
        }

        match self.tokens.peek() {
            Some(token) => {
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

    fn consume(&mut self) -> Option<Node<Token>> {
        self.last_token_span = match self.peek() {
            Some(tok) => tok.span,
            None => DUMMY_SPAN,
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

    fn eat_symbol(&mut self) -> Option<Node<Symbol>> {
        self.expected_tokens.push(TokenKind::Ident);
        match self.peek() {
            Some(&Node { value: Token::Ident(_), .. }) => { }
            _ => return None,
        }

        match self.consume() {
            Some(Node { value: Token::Ident(ident), span }) => {
                Some(Node::new(ident, span))
            }
            _ => {
                panic!("Parser::peek and Parser::consume returned different results");
            }
        }
    }
    
    fn eat_unqualified_name(&mut self) -> Option<Node<String>> {
        self.expected_tokens.push(TokenKind::Ident);
        match self.peek() {
            Some(&Node { value: Token::Ident(Symbol::Unqualified(_)), .. }) => { }
            _ => return None,
        }

        match self.consume() {
            Some(Node { value: Token::Ident(Symbol::Unqualified(name)), span }) => {
                Some(Node::new(name, span))
            }
            _ => {
                panic!("Parser::peek and Parser::consume returned different results");
            }
        }
    }

    fn eat_cased_name(&mut self, uppercase: bool) -> Option<Node<String>> {
        match self.peek() {
            Some(&Node { value: Token::Ident(Symbol::Unqualified(ref name)), .. }) => {
                if name.chars().nth(0).unwrap().is_uppercase() != uppercase {
                    return None;
                }
            }
            _ => {
                return None;
            }
        }

        match self.consume() {
            Some(Node { value: Token::Ident(Symbol::Unqualified(name)), span }) => {
                debug_assert_eq!(name.chars().nth(0).unwrap().is_uppercase(), uppercase);
                Some(Node::new(name, span))
            }
            _ => {
                panic!("Parser::peek and Parser::consume returned different results");
            }
        }
    }
    
    fn eat_var_name(&mut self) -> Option<Node<String>> {
        self.expected_tokens.push(TokenKind::VarName);
        self.eat_cased_name(false)
    }
    
    fn eat_tag_name(&mut self) -> Option<Node<String>> {
        self.expected_tokens.push(TokenKind::TagName);
        self.eat_cased_name(true)
    }
    
    fn eat_literal(&mut self) -> Option<Node<Literal>> {
        self.expected_tokens.push(TokenKind::Literal);
        match self.peek() {
            Some(&Node { value: Token::Int(_), .. }) |
            Some(&Node { value: Token::Float(_), .. }) |
            Some(&Node { value: Token::Bool(_), .. }) => { }
            _ => return None,
        }

        match self.consume() {
            Some(Node { value: Token::Int(int), span }) => {
                Some(Node::new(Literal::Int(int), span))
            }
            Some(Node { value: Token::Float(float), span }) => {
                Some(Node::new(Literal::Float(float), span))
            }
            Some(Node { value: Token::Bool(b), span }) => {
                Some(Node::new(Literal::Bool(b), span))
            }
            _ => {
                panic!("Parser::peek and Parser::consume returned different results");
            }
        }
    }

    fn eat_operator(&mut self) -> Option<Node<Symbol>> {
        self.expected_tokens.push(TokenKind::Operator);
        match self.peek() {
            Some(&Node { value: Token::Operator(_), .. }) => { }
            _ => return None,
        }

        match self.consume() {
            Some(Node { value: Token::Operator(op), span }) => {
                Some(Node::new(op, span))
            }
            _ => {
                panic!("Parser::peek and Parser::consume returned different results");
            }
        }
    }
    
    fn eat_unqualified_operator(&mut self) -> Option<Node<String>> {
        self.expected_tokens.push(TokenKind::UnqualifiedOperator);
        match self.peek() {
            Some(&Node { value: Token::Operator(Symbol::Unqualified(_)), .. }) => { }
            _ => return None,
        }

        match self.consume() {
            Some(Node { value: Token::Operator(Symbol::Unqualified(op)), span }) => {
                Some(Node::new(op, span))
            }
            _ => {
                panic!("Parser::peek and Parser::consume returned different results");
            }
        }
    }
    
    fn expr_term(&mut self) -> Option<ParseResult<ExprNode>> {
        if let Some(Node { value, span }) = self.eat_symbol() {
            return Some(Ok(Node::new(Expr::Ident(value), span)));
        }

        if let Some(Node { value, span }) = self.eat_literal() {
            return Some(Ok(Node::new(Expr::Literal(value), span)));
        }

        if self.eat(Token::OpenParen) {
            let start_span = self.previous_span();

            let expr = match self.eat_operator() {
                Some(operator) => {
                    return if self.eat(Token::CloseParen) {
                        // got something like (+)
                        let span = start_span.merge(self.previous_span());
                        Some(Ok(Node::new(Expr::Ident(operator.value), span)))
                    } else {
                        match self.application() {
                            Ok(expr) => {
                                if !self.eat(Token::CloseParen) {
                                    self.emit_error();
                                    return Some(Err(()));
                                }
                                let span = start_span.merge(self.previous_span());
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
                let node_span = start_span.merge(self.previous_span());
                let expr = Expr::Parenthesised(Box::new(expr));
                return Some(Ok(Node::new(expr, node_span)));
            } else if self.eat(Token::Comma) {
                return Some(self.finish_tuple_expr(expr, start_span));
            } else {
                self.emit_error();
                return Some(Err(()));
            }
        }

        if self.eat(Token::OpenBracket) {
            return Some(self.list_expr());
        }

        None
    }

    fn finish_tuple_expr(&mut self, first: Node<Expr>, paren: Span) -> ParseResult<ExprNode> {
        let mut items = vec![first, try!(self.expr(false))];
        loop {
            if self.eat(Token::CloseParen) {
                let span = paren.merge(self.previous_span());
                let expr = Expr::Tuple(items);
                return Ok(Node::new(expr, span));
            }
            
            if !self.eat(Token::Comma) {
                self.emit_error();
                return Err(());
            }

            items.push(try!(self.expr(false)));
        }
    }

    fn list_expr(&mut self) -> ParseResult<ExprNode> {
        let start_span = self.previous_span();
        let mut items = Vec::new();
        loop {
            if self.eat(Token::CloseBracket) {
                break;
            }
            items.push(try!(self.expr(false)));
            if self.eat(Token::CloseBracket) {
                break;
            }
            if !self.eat(Token::Comma) {
                self.emit_error();
                return Err(());
            }
        }
        let span = start_span.merge(self.previous_span());
        let expr = Expr::List(items);
        Ok(Node::new(expr, span))
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
        let mut span = function.span;

        while let Some(result) = self.expr_term() {
            let expr = try!(result);
            span = span.merge(expr.span);
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
                let span = start_span.merge(self.next_span());
                let operator = Node::new(operator.value, operator.span);
                return Ok(operator_left_section(operator, result, span));
            } else if can_section {
                // try to eat to add it to expected token list
                assert!(!self.eat(Token::CloseParen));
                can_section = false;
            }

            let rhs = try!(self.application());
            let span = result.span.merge(rhs.span);
            let rhs = Box::new(rhs);
            let lhs = Box::new(result);
            result = Node::new(Expr::Infix(lhs, operator, rhs), span);
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

        if let Some(Node { value, span }) = self.eat_symbol() {
            return Some(Ok(Node::new(Pattern::from_symbol(value, span), span)));
        }

        if let Some(Node { value, span }) = self.eat_literal() {
            return Some(Ok(Node::new(Pattern::Literal(value), span)));
        }

        if self.eat(Token::OpenParen) {
            let start_span = self.previous_span();
            
            let pattern = match self.pattern() {
                Ok(pattern) => pattern,
                Err(_) => return Some(Err(())),
            };
            
            if self.eat(Token::CloseParen) {
                let node_span = start_span.merge(self.previous_span());
                let pattern = Pattern::Parenthesised(Box::new(pattern));
                return Some(Ok(Node::new(pattern, node_span)));
            } else if self.eat(Token::Comma) {
                return Some(self.finish_tuple_pattern(pattern, start_span));
            } else {
                self.emit_error();
                return Some(Err(()));
            }
        }

        if self.eat(Token::OpenBracket) {
            return Some(self.list_pattern());
        }

        None
    }

    fn finish_tuple_pattern(&mut self, first: Node<Pattern>, paren: Span) -> ParseResult<PatternNode> {
        let mut items = vec![first, try!(self.pattern())];
        loop {
            if self.eat(Token::CloseParen) {
                let span = paren.merge(self.previous_span());
                let pattern = Pattern::Tuple(items);
                return Ok(Node::new(pattern, span));
            }
            
            if !self.eat(Token::Comma) {
                self.emit_error();
                return Err(());
            }

            items.push(try!(self.pattern()));
        }
    }

    fn list_pattern(&mut self) -> ParseResult<PatternNode> {
        let start_span = self.previous_span();
        let mut items = Vec::new();
        loop {
            if self.eat(Token::CloseBracket) {
                break;
            }
            items.push(try!(self.pattern()));
            if self.eat(Token::CloseBracket) {
                break;
            }
            if !self.eat(Token::Comma) {
                self.emit_error();
                return Err(());
            }
        }
        let span = start_span.merge(self.previous_span());
        let pattern = Pattern::List(items);
        Ok(Node::new(pattern, span))
    }

    fn deconstruct(&mut self) -> ParseResult<PatternNode> {
        let tag_node = match self.eat_symbol() {
            Some(Node { value, span }) => {
                match Pattern::from_symbol(value, span) {
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
        let mut span = tag_node.span;

        while let Some(result) = self.pattern_term() {
            let pattern = try!(result);
            span = span.merge(pattern.span);
            args.push(pattern);
        }

        let pattern = Node::new(Pattern::Deconstruct(tag_node, args), span);
        if self.eat(Token::As) {
            match self.eat_unqualified_name() {
                Some(name) => {
                    let node_span = pattern.span.merge(name.span);
                    let pattern = Pattern::As(Box::new(pattern), name);
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
            let span = result.span.merge(rhs.span);
            let lhs = Box::new(result);
            result = Node::new(Pattern::Infix(lhs, operator, rhs), span);    
        }
    }
    
    fn if_(&mut self) -> ParseResult<ExprNode> {
        let start_span = self.previous_span();

        let condition = try!(self.expr(false));
        
        try!(self.expect(Token::Then));

        let then_branch = try!(self.expr(false));

        try!(self.expect(Token::Else));

        let else_branch = try!(self.expr(false));
        let span = start_span.merge(else_branch.span);
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
                let span = start_span.merge(expr.span);
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
                    assert!(self.case_branch().is_err());
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
                    span = span.merge(branch.span);
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

    fn case_branch(&mut self) -> ParseResult<Node<CaseBranch>> {
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
                let span = pattern.span.merge(expr.span);
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
                    assert!(!self.let_decl(true).is_ok());
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
                    span = span.merge(decl.span);
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
                span = span.merge(expr.span);
                Ok(Node::new(Expr::Let(decls, Box::new(expr)), span))
            }
            Err(_) => {
                Err(())
            }
        }
    }

    fn let_decl(&mut self, allow_type: bool) -> ParseResult<Node<LetDecl>> {
        let is_paren = match self.peek() {
            Some(&Node { value: Token::OpenParen, .. }) => true,
            _ => false,
        };
        let is_op = match self.peek2() {
            Some(&Node { value: Token::Operator(_), .. }) => true,
            _ => false,
        };

        if is_paren && is_op {
            assert!(self.eat(Token::OpenParen));
            let symbol = match self.eat_unqualified_operator() {
                Some(op) => op,
                None => {
                    self.emit_error();
                    return Err(());
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
                Some(name) => {
                    self.type_or_def(name, allow_type)
                }
                None => {
                    assert!(!self.eat(Token::OpenParen));
                    self.emit_error();
                    Err(())
                }
            }
        }
    }

    fn scheme(&mut self) -> ParseResult<Node<Scheme>> {
        if self.eat(Token::OpenParen) {
            let mut constraints = Vec::new();
            loop {
                let var = match self.eat_var_name() {
                    Some(name) => name,
                    None => {
                        self.emit_error();
                        return Err(());
                    }
                };
                try!(self.expect(Token::Colon));
                let bound = match self.eat_symbol() {
                    Some(symbol) => symbol,
                    None => {
                        self.emit_error();
                        return Err(());
                    }
                };
                if self.eat(Token::CloseParen) {
                    break;
                } else {
                    try!(self.expect(Token::Comma));
                }
                constraints.push((var, bound));
            }
            try!(self.expect(Token::FatArrow));
            match self.type_() {
                Ok(type_) => {
                    let span = constraints[0].0.span.merge(type_.span);
                    let scheme = Scheme {
                        bounds: constraints,
                        type_: type_,
                    };
                    Ok(Node::new(scheme, span))
                }
                Err(_) => Err(()),
            }
        } else {
            self.type_().map(|type_| {
                let span = type_.span;
                let scheme = Scheme {
                    bounds: Vec::new(),
                    type_: type_,
                };
                Node::new(scheme, span)
            })
        }
    }

    fn type_term(&mut self) -> Option<ParseResult<TypeNode>> {
        if let Some(sym) = self.eat_symbol() {
            return Some(Ok(sym.map(Type::from_symbol)));
        }

        if self.eat(Token::Self_) {
            let span = self.previous_span();
            Some(Ok(Node::new(Type::SelfType, span)))
        } else if self.eat(Token::OpenParen) {
            let start_span = self.previous_span();
            let type_ = match self.type_() {
                Ok(type_) => type_,
                Err(()) => return Some(Err(())),
            };

            if self.eat(Token::Comma) {
                Some(self.finish_tuple_type(type_, start_span))
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

    fn finish_tuple_type(&mut self, first: Node<Type>, paren: Span) -> ParseResult<TypeNode> {
        let mut items = vec![first, try!(self.type_())];
        loop {
            if self.eat(Token::CloseParen) {
                let span = paren.merge(self.previous_span());
                let type_ = Type::Tuple(items);
                return Ok(Node::new(type_, span));
            }
            
            if !self.eat(Token::Comma) {
                self.emit_error();
                return Err(());
            }

            items.push(try!(self.type_()));
        }
    }

    fn type_application(&mut self) -> ParseResult<TypeNode> {
        let mut result = match self.type_term() {
            Some(Ok(type_)) => type_,
            Some(Err(_)) => return Err(()),
            None => {
                self.emit_error();
                return Err(());
            }
        };

        while let Some(res) = self.type_term() {
            let type_ = try!(res);
            let span = result.span.merge(type_.span);
            result = Node::new(Type::Apply(Box::new(result), Box::new(type_)), span);
        }

        Ok(result)
    }

    fn type_(&mut self) -> ParseResult<TypeNode> {
        let arg = try!(self.type_application());

        if self.eat(Token::Arrow) {
            match self.type_() {
                Ok(result) => {
                    let span = arg.span.merge(result.span);
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

    fn pattern_assign(&mut self) -> ParseResult<Node<Def>> {
        let first = match self.pattern_term() {
            Some(Ok(pattern)) => pattern,
            Some(Err(_)) | None => return Err(()),
        };

        if self.eat(Token::Equals) {
            match self.expr(false) {
                Ok(expr) => {
                    let span = first.span.merge(expr.span);
                    Ok(Node::new(Def {
                        pattern: first,
                        value: Some(expr),
                    }, span))
                }
                Err(()) => {
                    let span = first.span;
                    Ok(Node::new(Def {
                        pattern: first,
                        value: None,
                    }, span))
                }
            }
        } else {
            let op = try!(self.eat_unqualified_operator().ok_or(()));
            let second = try!(self.pattern_term().ok_or(()).and_then(|x| x));

            try!(self.expect(Token::Equals));

            let pattern = op.map(Pattern::Var);
            let params = vec![first, second];
            let span;

            let expr = match self.expr(false) {
                Ok(expr) => {
                    let lambda_span = params[0].span.merge(expr.span);
                    span = lambda_span;
                    Some(Node::new(Expr::Lambda(params, Box::new(expr)), lambda_span))
                }
                Err(_) => {
                    span = pattern.span;
                    None
                }
            };
            
            Ok(Node::new(Def {
                pattern: pattern,
                value: expr,
            }, span))
        }
    }

    fn type_or_def(&mut self, symbol: Node<String>, allow_type: bool) -> ParseResult<Node<LetDecl>> {
        if allow_type && self.eat(Token::Colon) {
            let (scheme, span) = match self.scheme() {
                Ok(scheme) => {
                    let span = symbol.span.merge(scheme.span);
                    (Some(scheme), span)
                }
                Err(_) => {
                    (None, symbol.span)
                }
            };
            Ok(Node::new(LetDecl::Type(TypeAnnot {
                value: symbol,
                type_: scheme,
            }), span))
        } else {
            let mut params = Vec::new();
            let pattern = symbol.map(Pattern::Var);
            loop {
                match self.pattern_term() {
                    Some(Ok(pattern)) => params.push(pattern),
                    Some(Err(_)) => {
                        let span = pattern.span;
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
                            let span = pattern.span;
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
                    span = pattern.span.merge(expr.span);
                    if params.is_empty() {
                        Some(expr)
                    } else {
                        let lambda_span = params[0].span.merge(expr.span);
                        Some(Node::new(Expr::Lambda(params, Box::new(expr)), lambda_span))
                    }
                }
                Err(_) => {
                    span = pattern.span;
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
                let span = expr.span;
                Ok(Node::new(Expr::Do(Box::new(expr)), span))
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

    fn do_statement(&mut self) -> Option<ParseResult<Node<DoExpr>>> {
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
                            let span = pattern.span.merge(expr.span);
                            let e = DoExpr::Bind(pattern, expr, Box::new(rest));
                            Some(Ok(Node::new(e, span)))
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
                        None => {
                            let span = expr.span;
                            return Some(Ok(Node::new(DoExpr::Done(expr), span)));
                        }
                    };
                    let span = expr.span.merge(rest.span);
                    let e = DoExpr::Sequence(expr, Box::new(rest));
                    Some(Ok(Node::new(e, span)))
                }
                Err(_) => {
                    Some(Err(()))
                }
            }
        }
    }

    fn do_if(&mut self) -> ParseResult<Node<DoExpr>> {
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

        let span = if_span.merge(rest.span);
        let e = DoExpr::If(cond, Box::new(rest));
        Ok(Node::new(e, span))
    }

    fn do_let(&mut self) -> ParseResult<Node<DoExpr>> {
        let let_span = self.previous_span();

        let Node { value: Def { pattern, value }, .. } = try!(self.pattern_assign());

        let value = try!(value.ok_or(()));

        let rest = match self.do_statement() {
            Some(Ok(rest)) => rest,
            Some(Err(_)) => return Err(()),
            None => {
                self.emit_error();
                return Err(());
            }
        };

        let span = let_span.merge(rest.span);
        let e = DoExpr::Let(pattern, value, Box::new(rest));
        Ok(Node::new(e, span))
    }

    fn fixity(&mut self, associativity: Associativity) -> ParseResult<Node<Decl>> {
        let span = self.previous_span();

        match self.peek() {
            Some(&Node { value: Token::Int(_), .. }) => { }
            _ => {
                self.expected_tokens.push(TokenKind::Int);
                self.emit_error();
                return Err(());
            }
        }

        let precedence = match self.consume() {
            Some(Node { value: Token::Int(precedence), span }) => {
                Node::new(precedence, span)
            }
            _ => {
                panic!("Parser::peek and Parser::consume returned different reults");
            }
        };

        let op = match self.eat_unqualified_operator() {
            Some(op) => op,
            None => {
                self.emit_error();
                return Err(());
            }
        };

        let span = span.merge(self.previous_span());

        Ok(Node::new(Decl::Infix(associativity, op, precedence), span))
    }

    fn type_decl(&mut self) -> ParseResult<Node<Decl>> {
        let span = self.previous_span();

        let is_alias = self.eat(Token::Alias);

        let name = match self.eat_unqualified_name() {
            Some(name) => name,
            None => return Err(()),
        };

        let mut vars = Vec::new();
        while let Some(name) = self.eat_var_name() {
            vars.push(name);
        }

        try!(self.expect(Token::Equals));

        if is_alias {
            let type_ = self.type_().ok();
            let span = span.merge(self.previous_span());
            Ok(Node::new(Decl::TypeAlias(TypeAlias {
                name: name,
                vars: vars,
                type_: type_,
            }), span))
        } else if self.eat(Token::OpenBrace) {
            let fields = self.record().unwrap_or_else(|_| Vec::new());
            let span = span.merge(self.previous_span());
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
            let span = span.merge(self.previous_span());
            Ok(Node::new(Decl::Union(UnionType {
                name: name,
                vars: vars,
                cases: cases,
            }), span))
        }
    }

    fn record(&mut self) -> ParseResult<Vec<(Node<String>, Node<Type>)>> {
        let mut fields = Vec::new();
        while !self.eat(Token::CloseBrace) {
            let name = match self.eat_unqualified_name() {
                Some(name) => name,
                None => {
                    self.emit_error();
                    return Err(());
                }
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

    fn union_case(&mut self) -> ParseResult<Node<UnionCase>> {
        let name = match self.eat_tag_name() {
            Some(name) => name,
            None => {
                self.emit_error();
                return Err(());
            }
        };
        let mut args = Vec::new();
        while let Some(result) = self.type_term() {
            args.push(try!(result));
        }

        let span = if args.is_empty() {
            name.span
        } else {
            name.span.merge(args[args.len() - 1].span)
        };

        Ok(Node::new(UnionCase {
            tag: name,
            args: args,
        }, span))
    }

    fn trait_(&mut self) -> ParseResult<Node<Trait>> {
        let span = self.previous_span();

        let name = match self.eat_unqualified_name() {
            Some(name) => name,
            None => {
                self.emit_error();
                return Err(());
            }
        };

        let mut base_traits = Vec::new();
        if self.eat(Token::Colon) {
            loop {
                match self.eat_symbol() {
                    Some(symbol) => base_traits.push(symbol),
                    None => {
                        self.emit_error();
                        return Err(());
                    }
                }
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

        let span = span.merge(self.previous_span());
        Ok(Node::new(Trait {
            name: name,
            base_traits: base_traits,
            values: values,
        }, span))
    }

    fn type_annot(&mut self) -> Option<ParseResult<Node<TypeAnnot>>> {
        let symbol = if self.eat(Token::OpenParen) {
            let span = self.previous_span();
            let name = match self.eat_unqualified_operator() {
                Some(op) => op.value,
                None => return Some(Err(())),
            };
            if !self.eat(Token::CloseParen) {
                return Some(Err(()));
            }
            let span = span.merge(self.previous_span());
            Node::new(name, span)
        } else {
            match self.eat_unqualified_name() {
                Some(name) => name,
                None => {
                    return if self.peek().is_none() {
                        None
                    } else {
                        self.emit_error();
                        Some(Err(()))
                    };
                }
            }
        };

        if !self.eat(Token::Colon) {
            self.emit_error();
            return Some(Err(()));
        }

        let (type_, span) = match self.scheme() {
            Ok(scheme) => {
                let span = symbol.span.merge(scheme.span);
                (Some(scheme), span)
            }
            Err(()) => (None, symbol.span),
        };

        Some(Ok(Node::new(TypeAnnot {
            value: symbol,
            type_: type_,
        }, span)))
    }

    fn impl_(&mut self) -> ParseResult<Node<Impl>> {
        let span = self.previous_span();
        let scheme = try!(self.scheme());
        try!(self.expect(Token::Colon));
        let trait_ = match self.eat_symbol() {
            Some(symbol) => symbol,
            None => {
                self.emit_error();
                return Err(());
            }
        };
        try!(self.expect(Token::Where));
        let mut values = Vec::new();
        let old_indent = self.align_on_next();

        loop {
            self.accept_aligned = true;
            if self.peek().is_none() {
                break;
            }

            match self.let_decl(false) {
                Ok(Node { value, span, .. }) => {
                    match value {
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

        let span = span.merge(self.previous_span());
        
        Ok(Node::new(Impl {
            scheme: scheme,
            trait_: trait_,
            values: values,
        }, span))
    }

    fn decl(&mut self) -> ParseResult<Node<Decl>> {
        if self.eat(Token::Type) {
            self.type_decl()
        } else if self.eat(Token::Trait) {
            self.trait_().map(|n| n.map(Decl::Trait))
        } else if self.eat(Token::Impl) {
            self.impl_().map(|n| n.map(Decl::Impl))
        } else if self.eat(Token::Infixl) {
            self.fixity(Associativity::Left)
        } else if self.eat(Token::Infixr) {
            self.fixity(Associativity::Right)
        } else if self.eat(Token::Infix) {
            self.fixity(Associativity::None)
        } else {
            self.let_decl(true).map(|n| n.map(Decl::Let))
        }
    }

    fn exposed_item_list(&mut self) -> ParseResult<Node<ItemList<Node<ExposedItem>>>> {
        let mut items = Vec::new();
        let span_start = self.previous_span();

        if self.eat(Token::DotDot) {
            try!(self.expect(Token::CloseParen));
            let span = span_start.merge(span_start);
            return Ok(Node::new(ItemList::All, span));
        }

        loop {
            let name = match self.eat_unqualified_name() {
                Some(name) => name,
                None => {
                    self.emit_error();
                    return Err(());
                }
            };

            let subitems = if self.eat(Token::OpenParen) {
                Some(try!(self.exposed_subitem_list()))
            } else {
                None
            };

            let span = name.span.merge(self.previous_span());

            let item = ExposedItem {
                name: name,
                subitems: subitems,
            };

            items.push(Node::new(item, span));

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

        let span = span_start.merge(self.previous_span());
        Ok(Node::new(ItemList::Some(items), span))
    }

    fn exposed_subitem_list(&mut self) -> ParseResult<Node<ItemList<Node<String>>>> {
        let mut subitems = Vec::new();
        let span_start = self.previous_span();
        
        if self.eat(Token::DotDot) {
            try!(self.expect(Token::CloseParen));
            let span = span_start.merge(self.previous_span());
            return Ok(Node::new(ItemList::All, span));
        }

        loop {
            let (name, span) = match self.eat_unqualified_name() {
                Some(Node { value, span }) => (value, span),
                None => {
                    self.emit_error();
                    return Err(());
                }
            };

            subitems.push(Node::new(name, span));

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

        let span = span_start.merge(self.previous_span());

        Ok(Node::new(ItemList::Some(subitems), span))
    }

    fn module_def(&mut self) -> ParseResult<Node<ModuleDef>> {
        try!(self.expect(Token::Module));
        let span_start = self.previous_span();

        let name = match self.eat_symbol() {
            Some(sym) => sym.map(Symbol::full_name),
            None => {
                self.emit_error();
                return Err(());
            }
        };

        if name.value != self.module {
            self.bad_module_name();
            return Err(());
        }

        try!(self.expect(Token::Exposing));
        try!(self.expect(Token::OpenParen));

        let items = try!(self.exposed_item_list());
        let span = span_start.merge(self.previous_span());

        Ok(Node::new(ModuleDef {
            name: name,
            exposing: items,
        }, span))
    }

    fn import(&mut self) -> ParseResult<Node<Import>> {
        let span_start = self.previous_span();

        let name = match self.eat_symbol() {
            Some(sym) => sym.map(Symbol::full_name),
            None => {
                self.emit_error();
                return Err(());
            }
        };

        let alias = if self.eat(Token::As) {
            match self.eat_symbol() {
                Some(sym) => Some(sym.map(Symbol::full_name)),
                None => {
                    self.emit_error();
                    return Err(());
                }
            }
        } else {
            None
        };

        let exposing = if self.eat(Token::Exposing) {
            try!(self.expect(Token::OpenParen));
            Some(try!(self.exposed_item_list()))
        } else {
            None
        };

        let span = span_start.merge(self.previous_span());

        Ok(Node::new(Import {
            name: name,
            alias: alias,
            exposing: exposing,
        }, span))
    }

    fn module(&mut self, require_def: bool) -> ParseResult<Module> {
        self.accept_aligned = true;
        self.current_indent = 1;

        let def = if require_def {
            match self.module_def() {
                Ok(def) => def,
                Err(()) => return Err(()),
            }
        } else {
            let span = Span::new(Position::new(1, 1), Position::new(1, 1));
            let def = ModuleDef {
                name: Node::new(self.module.to_string(), span),
                exposing: Node::new(ItemList::Some(Vec::new()), DUMMY_SPAN),
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

fn operator_right_section(operator: Node<Symbol>, expr: ExprNode, span: Span) -> ExprNode {
    let pattern = Pattern::Var("#".to_string());
    let pattern_node = Node::new(pattern, operator.span);
    let value = Expr::Ident(Symbol::Unqualified("#".to_string()));
    let value_node = Node::new(value, operator.span);
    let body = Expr::Infix(Box::new(value_node), operator, Box::new(expr));
    let body_node = Node::new(body, span);
    let lambda = Expr::Lambda(vec![pattern_node], Box::new(body_node));
    Node::new(lambda, span)
}

fn operator_left_section(operator: Node<Symbol>, expr: ExprNode, span: Span) -> ExprNode {
    let pattern = Pattern::Var("#".to_string());
    let pattern_node = Node::new(pattern, operator.span);
    let value = Expr::Ident(Symbol::Unqualified("#".to_string()));
    let value_node = Node::new(value, operator.span);
    let body = Expr::Infix(Box::new(expr), operator, Box::new(value_node));
    let body_node = Node::new(body, span);
    let lambda = Expr::Lambda(vec![pattern_node], Box::new(body_node));
    Node::new(lambda, span)
}

#[cfg(test)]
mod tests {
    use parsing::lexer::lex;
    use ast::Literal;
    use ast::parsed::{Expr, Pattern, Type, Scheme, LetDecl, Def, TypeAnnot, Symbol, DoExpr};
    use super::Parser;

    fn write_symbol(symbol: &Symbol, output: &mut String) {
        match *symbol {
            Symbol::Qualified(ref path, ref name) => {
                output.push_str(path);
                output.push_str(".");
                output.push_str(name);
            }
            Symbol::Unqualified(ref name) => {
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

    fn write_do_expr(expr: &DoExpr, output: &mut String) {
        match *expr {
            DoExpr::Bind(ref pat, ref expr, ref rest) => {
                output.push_str("(do-bind ");
                write_expr(&expr.value, output);
                output.push(' ');
                write_pattern(&pat.value, output);
                output.push(' ');
                write_do_expr(&rest.value, output);
                output.push(')');
            }
            DoExpr::Done(ref expr) => {
                output.push_str("(do ");
                write_expr(&expr.value, output);
                output.push(')');
            }
            DoExpr::If(ref cond, ref rest) => {
                output.push_str("(do-if ");
                write_expr(&cond.value, output);
                output.push(' ');
                write_do_expr(&rest.value, output);
                output.push(')');
            }
            DoExpr::Let(ref pat, ref expr, ref rest) => {
                output.push_str("(do-let ");
                write_expr(&expr.value, output);
                output.push(' ');
                write_pattern(&pat.value, output);
                output.push(' ');
                write_do_expr(&rest.value, output);
                output.push(')');
            }
            DoExpr::Sequence(ref expr, ref rest) => {
                output.push_str("(bind-ignore ");
                write_expr(&expr.value, output);
                output.push(' ');
                write_do_expr(&rest.value, output);
                output.push(')');
            }
        }
    }

    fn write_expr(expr: &Expr, output: &mut String) {
        match *expr {
            Expr::Apply(ref f, ref args) => {
                output.push_str("(apply ");
                write_expr(&f.value, output);
                for arg in args {
                    output.push(' ');
                    write_expr(&arg.value, output);
                }
                output.push(')');
            }
            Expr::Ident(ref symbol) => {
                write_symbol(symbol, output);
            }
            Expr::If(ref cond, ref then, ref else_) => {
                output.push_str("(if ");
                write_expr(&cond.value, output);
                output.push(' ');
                write_expr(&then.value, output);
                output.push(' ');
                write_expr(&else_.value, output);
                output.push(')');
            }
            Expr::Infix(ref lhs, ref op, ref rhs) => {
                output.push('(');
                write_symbol(&op.value, output);
                output.push(' ');
                write_expr(&lhs.value, output);
                output.push(' ');
                write_expr(&rhs.value, output);
                output.push(')');
            }
            Expr::Literal(ref literal) => {
                write_literal(literal, output);
            }
            Expr::Parenthesised(ref expr) => {
                output.push_str("(parens ");
                write_expr(&expr.value, output);
                output.push_str(")");
            }
            Expr::Lambda(ref params, ref value) => {
                output.push_str("(lambda (");
                let mut need_space = false;
                for param in params {
                    if need_space {
                        output.push(' ');
                    }
                    write_pattern(&param.value, output);
                    need_space = true;
                }
                output.push_str(") ");
                write_expr(&value.value, output);
                output.push_str(")");
            }
            Expr::Case(ref value, ref branches) => {
                output.push_str("(case ");
                write_expr(&value.value, output);
                for branch in branches {
                    output.push_str(" (");
                    write_pattern(&branch.value.pattern.value, output);
                    match branch.value.guard {
                        Some(ref guard) => {
                            output.push(' ');
                            write_expr(&guard.value, output);
                        }
                        None => { }
                    }
                    output.push(' ');
                    write_expr(&branch.value.value.value, output);
                    output.push(')');

                }
                output.push(')');
            }
            Expr::Let(ref decls, ref value) => {
                output.push_str("(let ");
                for decl in decls {
                    write_let_decl(&decl.value, output);
                    output.push(' ');
                }
                write_expr(&value.value, output);
                output.push(')');
            }
            Expr::Tuple(_) => {
                unimplemented!()
            }
            Expr::List(_) => {
                unimplemented!()
            }
            Expr::Do(ref do_) => {
                write_do_expr(&do_.value, output);
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
                write_symbol(&tag.value, output);
                for arg in args {
                    output.push(' ');
                    write_pattern(&arg.value, output);
                }
                output.push(')');
            }
            Pattern::Infix(ref lhs, ref op, ref rhs) => {
                output.push_str("(opdec ");
                write_symbol(&op.value, output);
                output.push(' ');
                write_pattern(&lhs.value, output);
                output.push(' ');
                write_pattern(&rhs.value, output);
                output.push(')');
            }
            Pattern::Literal(ref literal) => {
                write_literal(literal, output);
            }
            Pattern::Parenthesised(ref pattern) => {
                output.push_str("(parens ");
                write_pattern(&pattern.value, output);
                output.push_str(")");
            }
            Pattern::As(ref pattern, ref alias) => {
                output.push_str("(alias ");
                output.push_str(&alias.value);
                output.push_str(" ");
                write_pattern(&pattern.value, output);
                output.push_str(")");
            }
            Pattern::Tuple(_) => {
                unimplemented!()
            }
            Pattern::List(_) => {
                unimplemented!()
            }
        }
    }

    fn write_let_decl(decl: &LetDecl, output: &mut String) {
        match *decl {
            LetDecl::Def(Def { ref pattern, ref value }) => {
                output.push_str("(def ");
                write_pattern(&pattern.value, output);
                output.push(' ');
                write_expr(&value.as_ref().unwrap().value, output);
                output.push(')');
            }
            LetDecl::Type(TypeAnnot { ref value, ref type_ }) => {
                output.push_str("(typeannot ");
                output.push_str(&value.value);
                output.push(' ');
                write_scheme(&type_.as_ref().unwrap().value, output);
                output.push(')');
            }
        }
    }

    fn write_scheme(scheme: &Scheme, output: &mut String) {
        if scheme.bounds.is_empty() {
            write_type(&scheme.type_.value, output);
        } else {
            unimplemented!()
        }
    }

    fn write_type(type_: &Type, output: &mut String) {
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
                write_type(&a.value, output);
                output.push(' ');
                write_type(&b.value, output);
                output.push(')');
            }
            Type::Concrete(ref t) => {
                write_symbol(&t, output);
            }
            Type::Function(ref a, ref b) => {
                output.push_str("(fn ");
                write_type(&a.value, output);
                output.push(' ');
                write_type(&b.value, output);
                output.push(')');
            }
            Type::Tuple(_) => {
                unimplemented!()
            }
        }
    }

    fn check_expr(source: &str, expected: &str) {
        let (tokens, errors) = lex(source, "<test>");
        assert!(errors.is_empty());
        let mut parser = Parser::new(tokens.into_iter(), "<test>");
        let expr = parser.expr(false).ok().unwrap();
        let mut printed = String::new();
        write_expr(&expr.value, &mut printed);
        assert_eq!(expected, printed);
        assert!(parser.errors.is_empty());
    }

    fn check_pattern(source: &str, expected: &str) {
        let (tokens, errors) = lex(source, "<test>");
        assert!(errors.is_empty());
        let mut parser = Parser::new(tokens.into_iter(), "<test>");
        let pattern = parser.pattern().ok().unwrap();
        let mut printed = String::new();
        write_pattern(&pattern.value, &mut printed);
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
            "(do-bind value (var a) (do foo))");
    }
    
    #[test]
    fn do_if() {
        check_expr(
            "
do  a <- value
    let b = a
    if b
    c",
            "(do-bind value (var a) (do-let a (var b) (do-if b (do c))))");
    }
}
