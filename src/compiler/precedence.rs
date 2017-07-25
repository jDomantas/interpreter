use std::collections::HashMap;
use ast::{Node, Associativity, Name};
use ast::resolved::{Expr, Pattern, Def, Impl, Sym, Symbol, Items, CaseBranch};
use errors::Errors;


struct Context<'a, 'b> {
    errors: &'b mut Errors,
    current_module: Option<Name>,
    symbol_names: &'a HashMap<Sym, String>,
    operators: &'a HashMap<Sym, (Associativity, u64)>, 
}

impl<'a, 'b> Context<'a, 'b> {
    fn new(
            operators: &'a HashMap<Sym, (Associativity, u64)>,
            errors: &'b mut Errors,
            symbol_names: &'a HashMap<Sym, String>) -> Self {
        Context {
            errors,
            operators,
            symbol_names,
            current_module: None,
        }
    }

    fn get_precedence(&self, symbol: Symbol) -> (Associativity, u64) {
        match symbol {
            Symbol::Known(sym) => {
                self.operators.get(&sym).cloned().unwrap_or((Associativity::Left, 9))
            }
            Symbol::Unknown => {
                // lowest precedence will split expressions apart,
                // less errors should be reported in this case
                (Associativity::Left, 0)
            }
        }
    }

    fn precedence_error(&mut self, left: &Node<Symbol>, right: &Node<Symbol>) {
        let (le, ri) = match (left.value, right.value) {
            (Symbol::Known(l), Symbol::Known(r)) => (l, r),
            _ => return,
        };
        let (a1, p1) = self.get_precedence(left.value);
        let (a2, p2) = self.get_precedence(right.value);
        debug_assert_eq!(p1, p2);
        debug_assert!(a1 != a2 || a1 == Associativity::None || a2 == Associativity::None);
        debug_assert!(left.value != right.value || a1 == Associativity::None);
        let message = if left.value == right.value {
            format!("Operator `{}` is {}.",
                self.symbol_names[&le],
                a1.as_str())
        } else {
            format!("Operator `{}` is {}, and `{}` is {}.",
                self.symbol_names[&le],
                a1.as_str(),
                self.symbol_names[&ri],
                a2.as_str())
        };
        let span = left.span.merge(right.span);
        self.errors
            .fixity_error(self.current_module.as_ref().unwrap())
            .note(message, span)
            .done();
    }

    fn fix<T, F>(&mut self, mut args: Vec<Node<T>>, ops: Vec<Node<Symbol>>, builder: F) -> Node<T>
            where F: Fn(Node<T>, Node<Symbol>, Node<T>) -> Node<T> {
        debug_assert_eq!(ops.len() + 1, args.len());
        let mut output = Vec::new();
        let mut op_stack = Vec::<Node<Symbol>>::new();
        let mut arg_drain = args.drain(..);
        output.push(arg_drain.next().unwrap());
        for op in ops {
            let arg = arg_drain.next().unwrap();
            let (assoc, prec) = self.get_precedence(op.value);
            loop {
                let prec2 = if let Some(prev_op) = op_stack.last() {
                    let (assoc2, prec2) = self.get_precedence(prev_op.value);
                    if prec == prec2 &&
                        (assoc != assoc2 ||
                        assoc == Associativity::None ||
                        assoc2 == Associativity::None) {
                        self.precedence_error(prev_op, &op);
                    }
                    prec2
                } else {
                    break;
                };
                if prec < prec2 || (prec == prec2 && assoc == Associativity::Left) {
                    let rhs = output.pop().unwrap();
                    let lhs = output.pop().unwrap();
                    let prev = op_stack.pop().unwrap();
                    let expr = builder(lhs, prev, rhs);
                    output.push(expr);
                } else {
                    break;
                }
            }
            op_stack.push(op);
            output.push(arg);
        }
        debug_assert_eq!(op_stack.len() + 1, output.len());
        while let Some(op) = op_stack.pop() {
            let rhs = output.pop().unwrap();
            let lhs = output.pop().unwrap();
            let expr = builder(lhs, op, rhs);
            output.push(expr);
        }
        debug_assert_eq!(output.len(), 1);
        output.into_iter().next().unwrap()
    }

    fn fix_expr(&mut self, expr: Node<Expr>) -> Node<Expr> {
        let span = expr.span;
        expr.map(|expr| {
            match expr {
                Expr::Apply(a, b) => {
                    let a = Box::new(self.fix_expr(*a));
                    let b = Box::new(self.fix_expr(*b));
                    Expr::Apply(a, b)
                }
                Expr::Case(value, branches) => {
                    let value = Box::new(self.fix_expr(*value));
                    let branches = branches.into_iter().map(|branch| {
                        branch.map(|branch| {
                            let pattern = self.fix_pattern(branch.pattern);
                            let value = self.fix_expr(branch.value);
                            let guard = branch.guard.map(|e| self.fix_expr(e));
                            CaseBranch { pattern, value, guard }
                        })
                    }).collect();
                    Expr::Case(value, branches)
                }
                Expr::Ident(symbol) => {
                    Expr::Ident(symbol)
                }
                Expr::If(cond, then, else_) => {
                    let cond = Box::new(self.fix_expr(*cond));
                    let then = Box::new(self.fix_expr(*then));
                    let else_ = Box::new(self.fix_expr(*else_));
                    Expr::If(cond, then, else_)
                }
                Expr::Lambda(params, value) => {
                    let value = Box::new(self.fix_expr(*value));
                    Expr::Lambda(params, value)
                }
                Expr::Let(defs, value) => {
                    let defs = defs.into_iter().map(|def| {
                        def.map(|def| self.fix_def(def))
                    }).collect();
                    let value = Box::new(self.fix_expr(*value));
                    Expr::Let(defs, value)
                }
                Expr::List(items) => {
                    let items = items.into_iter().map(|i| self.fix_expr(i)).collect();
                    Expr::List(items)
                }
                Expr::Literal(literal) => {
                    Expr::Literal(literal)
                }
                Expr::Parenthesised(expr) => {
                    Expr::Parenthesised(expr)
                }
                Expr::Tuple(items) => {
                    let items = items.into_iter().map(|i| self.fix_expr(i)).collect();
                    Expr::Tuple(items)
                }
                e@Expr::Infix(_, _, _) => {
                    let mut exprs = Vec::new();
                    let mut ops = Vec::new();
                    let node = Node::new(e, span);
                    collect_expr(node, &mut ops, &mut exprs);
                    let exprs = exprs.into_iter().map(|e| self.fix_expr(e)).collect();
                    fn make_infix(lhs: Node<Expr>, op: Node<Symbol>, rhs: Node<Expr>) -> Node<Expr> {
                        let span = lhs.span.merge(rhs.span);
                        let expr = Expr::Infix(Box::new(lhs), op, Box::new(rhs));
                        Node::new(expr, span)
                    }
                    self.fix(exprs, ops, make_infix).value
                }
                Expr::DoIf(cond, rest) => {
                    let cond = Box::new(self.fix_expr(*cond));
                    let rest = Box::new(self.fix_expr(*rest));
                    Expr::DoIf(cond, rest)
                }
                Expr::Bind(pat, expr, rest) => {
                    let pat = self.fix_pattern(pat);
                    let expr = Box::new(self.fix_expr(*expr));
                    let rest = Box::new(self.fix_expr(*rest));
                    Expr::Bind(pat, expr, rest)
                }
            }
        })
    }

    fn fix_pattern(&mut self, pattern: Node<Pattern>) -> Node<Pattern> {
        let span = pattern.span;
        pattern.map(|pattern| {
            match pattern {
                Pattern::As(pat, alias) => {
                    let pat = Box::new(self.fix_pattern(*pat));
                    Pattern::As(pat, alias)
                }
                Pattern::Deconstruct(tag, params) => {
                    let params = params.into_iter().map(|param| {
                        self.fix_pattern(param)
                    }).collect();
                    Pattern::Deconstruct(tag, params)
                }
                Pattern::List(items) => {
                    let items = items.into_iter().map(|item| {
                        self.fix_pattern(item)
                    }).collect();
                    Pattern::List(items)
                }
                Pattern::Literal(literal) => {
                    Pattern::Literal(literal)
                }
                Pattern::Parenthesised(pat) => {
                    let pat = Box::new(self.fix_pattern(*pat));
                    Pattern::Parenthesised(pat)
                }
                Pattern::Tuple(items) => {
                    let items = items.into_iter().map(|item| {
                        self.fix_pattern(item)
                    }).collect();
                    Pattern::Tuple(items)
                }
                Pattern::Wildcard => {
                    Pattern::Wildcard
                }
                p@Pattern::Infix(_, _, _) => {
                    let mut patterns = Vec::new();
                    let mut ops = Vec::new();
                    let node = Node::new(p, span);
                    collect_pattern(node, &mut ops, &mut patterns);
                    let patterns = patterns.into_iter().map(|p| self.fix_pattern(p)).collect();
                    fn make_infix(lhs: Node<Pattern>, op: Node<Symbol>, rhs: Node<Pattern>) -> Node<Pattern> {
                        let span = lhs.span.merge(rhs.span);
                        let pattern = Pattern::Infix(Box::new(lhs), op, Box::new(rhs));
                        Node::new(pattern, span)
                    }
                    self.fix(patterns, ops, make_infix).value
                }
            }
        })
    }

    fn fix_def(&mut self, mut def: Def) -> Def {
        self.current_module = Some(def.module.clone());
        def.value = self.fix_expr(def.value);
        def
    }

    fn fix_impl(&mut self, mut impl_: Impl) -> Impl {
        impl_.values = impl_.values.into_iter().map(|def| {
            def.map(|def| self.fix_def(def))
        }).collect();
        impl_
    }
}

fn collect_expr(expr: Node<Expr>, ops: &mut Vec<Node<Symbol>>, exprs: &mut Vec<Node<Expr>>) {
    match expr.value {
        Expr::Infix(lhs, op, rhs) => {
            collect_expr(*lhs, ops, exprs);
            ops.push(op);
            collect_expr(*rhs, ops, exprs);
        }
        _ => {
            exprs.push(expr);
        }
    }
}

fn collect_pattern(pat: Node<Pattern>, ops: &mut Vec<Node<Symbol>>, pats: &mut Vec<Node<Pattern>>) {
    match pat.value {
        Pattern::Infix(lhs, op, rhs) => {
            collect_pattern(*lhs, ops, pats);
            ops.push(op);
            collect_pattern(*rhs, ops, pats);
        }
        _ => {
            pats.push(pat);
        }
    }
}

pub fn fix_items(items: Items, errors: &mut Errors) -> Items {
    let Items { types, items, traits, impls, annotations, fixities, symbol_names } = items;
    let (items, impls) = {
        let mut ctx = Context::new(&fixities, errors, &symbol_names);
        let items = items.into_iter().map(|i| ctx.fix_def(i)).collect();
        let impls = impls.into_iter().map(|i| ctx.fix_impl(i)).collect();
        (items, impls)
    };
    Items { types, items, traits, impls, annotations, fixities, symbol_names }
}
