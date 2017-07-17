use std::collections::HashMap;
use std::rc::Rc;
use ast::typed::{self as t, Type, Scheme};
use ast::resolved as r;
use ast::{Node, Literal};
use errors::Error;
use position::{Span, DUMMY_SPAN};


enum ConstraintSource {
    AlwaysStatisfied,
    IfCondition(Span),
    IfBranches(Span, Span),
    MatchBranches(usize, Span, Span),
    GuardCondition(Span),
    MatchedValue(Span),
    ListItems(usize, Span, Span),
    InfixOperator(Span),
    FunctionArg(usize, String, Span),
    Function(Span),
}

struct Constraint(Type, Type, ConstraintSource, String);

struct InferCtx {
    global_env: HashMap<String, Scheme>,
    local_env: Vec<(String, Scheme)>,
    next_var: u64,
    var_bounds: HashMap<u64, Vec<String>>,
    errors: Vec<Error>,
    constraints: Vec<Constraint>,
    current_module: String,
}

impl InferCtx {
    fn new() -> InferCtx {
        InferCtx {
            global_env: HashMap::new(),
            local_env: Vec::new(),
            next_var: 0,
            var_bounds: HashMap::new(),
            errors: Vec::new(),
            constraints: Vec::new(),
            current_module: String::new(),
        }
    }

    fn fresh_var(&mut self) -> u64 {
        let var = self.next_var;
        self.next_var += 1;
        var
    }

    fn add_var_bound(&mut self, var: u64, trait_: String) {
        self.var_bounds
            .entry(var)
            .or_insert_with(Vec::new)
            .push(trait_);
    }

    fn add_constraint(&mut self, a: &Type, b: &Type, source: ConstraintSource) {
        let constraint = Constraint(
            a.clone(),
            b.clone(),
            source,
            self.current_module.clone()
        );
        self.constraints.push(constraint);
    }

    fn add_local(&mut self, name: String, type_: Scheme) {
        self.local_env.push((name, type_));
    }

    fn restore_locals(&mut self, amount: usize) {
        while self.local_env.len() > amount {
            self.local_env.pop();
        }
    }

    fn instantiate_scheme(&mut self, scheme: &Scheme) -> Type {
        let mut substitution = HashMap::new();
        for var in &scheme.vars {
            let to = self.fresh_var();
            for bound in &var.bounds {
                self.add_var_bound(to, bound.clone());
            }
            substitution.insert(var.id, Type::Var(to));
        }
        scheme.type_.map_vars(&substitution)
    }

    fn infer_literal(&mut self, literal: &Literal) -> Type {
        let name = match *literal {
            Literal::Bool(_) => "Basics.Bool",
            Literal::Char(_) => "Basics.Char",
            Literal::Float(_) => "Basics.Float",
            Literal::Str(_) => "Basics.String",
            Literal::Int(_) => {
                let var = self.fresh_var();
                self.add_var_bound(var, "Basics.Number".to_string());
                return Type::Var(var);
            }
        };
        Type::Concrete(name.to_string())
    }

    fn infer_symbol(&mut self, symbol: &r::Symbol) -> (t::Symbol, Type) {
        let typed = t::Symbol::from_resolved(symbol.clone());
        let scheme = match *symbol {
            r::Symbol::Global(ref name) => self.global_env[name].clone(),
            r::Symbol::Local(ref name) => {
                self.local_env
                    .iter()
                    .rev()
                    .filter_map(|&(ref s, ref type_)| {
                        if s == name {Some(type_.clone()) } else { None }
                    })
                    .next()
                    .unwrap()
            }
            r::Symbol::Unknown => {
                let var = Type::Var(self.fresh_var());
                return (typed, var);
            }
        };
        (typed, self.instantiate_scheme(&scheme))
    }

    fn infer_pattern(&mut self, pattern: &Node<r::Pattern>) -> (Node<t::Pattern>, Type) {
        let (typed, type_) = match pattern.value {
            r::Pattern::As(ref pat, ref name) => {
                let (pat, type_) = self.infer_pattern(&**pat);
                self.add_local(name.value.clone(), empty_scheme(type_.clone()));
                (t::Pattern::As(Box::new(pat), name.clone()), type_)
            }
            r::Pattern::Deconstruct(ref _name, ref _args) => {
                unimplemented!()
            }
            r::Pattern::Infix(ref lhs, ref op, ref rhs) => {
                let span = lhs.span.merge(rhs.span);
                let pat = r::Pattern::Deconstruct(
                    op.clone(),
                    vec![(**lhs).clone(), (**rhs).clone()]
                );
                let node = Node::new(pat, span);
                let (pat, type_) = self.infer_pattern(&node);
                (pat.value, type_)
            }
            r::Pattern::List(ref items) => {
                let mut typed_items = Vec::new();
                let mut types = Vec::new();
                for item in items {
                    let (item, type_) = self.infer_pattern(item);
                    typed_items.push(item);
                    types.push(type_);
                }
                for i in 0..(types.len() - 1) {
                    let a = &types[i];
                    let b = &types[i + 1];
                    let a_span = typed_items[i].span;
                    let b_span = typed_items[i + 1].span;
                    let source = ConstraintSource::ListItems(
                        i,
                        a_span,
                        b_span
                    );
                    self.add_constraint(a, b, source);
                }
                let type_var = self.fresh_var();
                if let Some(ref t) = types.get(0) {
                    self.add_constraint(
                        t,
                        &Type::Var(type_var),
                        ConstraintSource::AlwaysStatisfied
                    );
                }
                let type_ = types.pop().unwrap_or_else(|| Type::Var(self.fresh_var()));
                let list = Type::Concrete("List.List".to_string());
                let type_ = Type::Apply(Rc::new(list), Rc::new(type_));
                (make_list_pattern(typed_items, type_var), type_)
            }
            r::Pattern::Literal(ref literal) => {
                let type_ = self.infer_literal(literal);
                (t::Pattern::Literal(literal.clone(), type_.clone()), type_)
            }
            r::Pattern::Parenthesised(ref pat) => {
                let (pat, type_) = self.infer_pattern(pat);
                (pat.value, type_)
            }
            r::Pattern::Tuple(ref items) => {
                let mut typed_items = Vec::new();
                let mut types = Vec::new();
                for item in items {
                    let (item, type_) = self.infer_pattern(item);
                    typed_items.push(item);
                    types.push(type_);
                }
                (t::Pattern::Tuple(typed_items), Type::Tuple(types))
            }
            r::Pattern::Var(ref name) => {
                let type_ = Type::Var(self.fresh_var());
                let pat = t::Pattern::Var(name.clone());
                self.add_local(name.clone(), empty_scheme(type_.clone()));
                (pat, type_)
            }
            r::Pattern::Wildcard => {
                let type_ = Type::Var(self.fresh_var());
                (t::Pattern::Wildcard, type_)
            }
        };
        (Node::new(typed, pattern.span), type_)
    }

    fn infer_do_expr(&mut self, expr: &Node<r::DoExpr>, var: Type) -> (Node<t::Expr>, Type) {
        let (typed, type_) = match expr.value {
            r::DoExpr::Bind(ref pat, ref val, ref rest) => {
                // val ~ m a
                // pat ~ a
                // rest ~ m b
                // result = m b
                
                unimplemented!()
            }
            r::DoExpr::Done(ref expr) => {
                // expr ~ m a
                // result = m a
                unimplemented!()
            }
            r::DoExpr::If(ref cond, ref rest) => {
                // cond ~ bool
                // rest ~ m a
                // result = m a
                unimplemented!()
            }
            r::DoExpr::Let(ref pat, ref val, ref rest) => {
                // ???
                unimplemented!()
            }
            r::DoExpr::Sequence(ref expr, ref rest) => {
                // expr ~ m a
                // rest ~ m b
                // result = m b
                unimplemented!()
            }
        };
        (Node::new(typed, expr.span), type_)
    }

    fn infer_expr(&mut self, expr: &Node<r::Expr>) -> (Node<t::Expr>, Type) {
        let (typed, type_) = match expr.value {
            r::Expr::Apply(ref a, ref b) => {
                let (fn_name, arg_pos) = function_name(&a.value);
                let (a, atype) = self.infer_expr(&**a);
                let (b, btype) = self.infer_expr(&**b);
                let var1 = Type::Var(self.fresh_var());
                let var2 = Type::Var(self.fresh_var());
                let source = ConstraintSource::FunctionArg(arg_pos, fn_name, b.span);
                self.add_constraint(&btype, &var2, source);
                let fn_type = Type::Function(Rc::new(var1), Rc::new(var2.clone()));
                let source = ConstraintSource::Function(a.span);
                self.add_constraint(&atype, &fn_type, source);
                let expr = t::Expr::Apply(Box::new(a), Box::new(b));
                (expr, var2)
            }
            r::Expr::Case(ref value, ref branches) => {
                let (value, type_) = self.infer_expr(&**value);
                let mut typed_branches = Vec::new();
                let mut last_branch = None;
                let locals_before = self.local_env.len();
                for (index, branch) in branches.iter().enumerate() {
                    let (branch, span) = (&branch.value, branch.span);
                    let (pat, pat_type) = self.infer_pattern(&branch.pattern);
                    let (val, val_type) = self.infer_expr(&branch.value);
                    let source = ConstraintSource::MatchedValue(pat.span);
                    self.add_constraint(&type_, &pat_type, source);
                    if let Some((type_, span)) = last_branch {
                        let source = ConstraintSource::MatchBranches(
                            index,
                            span,
                            branch.value.span
                        );
                        self.add_constraint(&type_, &val_type, source);
                    }
                    let guard = branch.guard.as_ref().map(|expr| {
                        let (typed, type_) = self.infer_expr(expr);
                        let source = ConstraintSource::GuardCondition(expr.span);
                        let bool_type = Type::Concrete("Basics.Bool".to_string());
                        self.add_constraint(&type_, &bool_type, source);
                        typed
                    });
                    last_branch = Some((val_type, branch.value.span));
                    typed_branches.push(Node::new(t::CaseBranch {
                        pattern: pat,
                        guard: guard,
                        value: val,
                    }, span));
                    self.restore_locals(locals_before);
                }
                let expr = t::Expr::Case(Box::new(value), typed_branches);
                (expr, last_branch.unwrap().0)
            }
            r::Expr::Do(ref do_) => {
                let var = self.fresh_var();
                self.add_var_bound(var, "Monad.Monad".to_string());
                let (expr, type_) = self.infer_do_expr(do_, Type::Var(var));
                (expr.value, type_)
            }
            r::Expr::Ident(ref name) => {
                let (symbol, type_) = self.infer_symbol(name);
                (t::Expr::Var(symbol, type_.clone()), type_)
            }
            r::Expr::If(ref cond, ref then, ref else_) => {
                let (cond, cond_type) = self.infer_expr(cond);
                let (then, then_type) = self.infer_expr(then);
                let (else_, else_type) = self.infer_expr(else_);
                let bool_type = Type::Concrete("Basics.Bool".to_string());
                let cond_source = ConstraintSource::IfCondition(cond.span);
                self.add_constraint(&cond_type, &bool_type, cond_source);
                let branch = ConstraintSource::IfBranches(
                    then.span,
                    else_.span
                );
                self.add_constraint(&then_type, &else_type, branch);
                (make_if(cond, then, else_), then_type)
            }
            r::Expr::Infix(ref lhs, ref op, ref rhs) => {
                let left_var = self.fresh_var();
                let right_var = self.fresh_var();
                let out_var = self.fresh_var();
                let op_type = Type::Function(
                    Rc::new(Type::Var(left_var)),
                    Rc::new(Type::Function(
                        Rc::new(Type::Var(right_var)),
                        Rc::new(Type::Var(out_var))
                    ))
                );
                let (typed_op, type_) = self.infer_symbol(&op.value);
                let (typed_lhs, ltype) = self.infer_expr(lhs);
                let (typed_rhs, rtype) = self.infer_expr(rhs);
                let source = ConstraintSource::InfixOperator(op.span);
                self.add_constraint(&op_type, &type_, source);
                let source = ConstraintSource::FunctionArg(
                    0,
                    op.value.clone().full_name(),
                    op.span
                );
                self.add_constraint(&Type::Var(left_var), &ltype, source);
                let source = ConstraintSource::FunctionArg(
                    1,
                    op.value.clone().full_name(),
                    op.span
                );
                self.add_constraint(&Type::Var(right_var), &rtype, source);
                let op_expr = t::Expr::Var(typed_op, op_type);
                let op = Node::new(op_expr, op.span);
                let span = lhs.span.merge(op.span);
                let first = Node::new(t::Expr::Apply(Box::new(op), Box::new(typed_lhs)), span);
                let type_ = Type::Var(out_var);
                (t::Expr::Apply(Box::new(first), Box::new(typed_rhs)), type_)
            }
            r::Expr::Lambda(ref params, ref value) => {
                let locals_before = self.local_env.len();
                let mut typed_params = Vec::new();
                let mut param_types = Vec::new();
                for param in params {
                    let (pat, type_) = self.infer_pattern(param);
                    typed_params.push(pat);
                    param_types.push(type_);
                }
                let (value, mut type_) = self.infer_expr(value);
                let expr = t::Expr::Lambda(typed_params, Box::new(value));
                for t in param_types.into_iter().rev() {
                    type_ = Type::Function(Rc::new(t), Rc::new(type_));
                }
                self.restore_locals(locals_before);
                (expr, type_)
            }
            r::Expr::Let(ref _defs, ref _types, ref _value) => {
                unimplemented!()
            }
            r::Expr::List(ref items) => {
                let mut typed_items = Vec::new();
                let mut types = Vec::new();
                for item in items {
                    let (item, type_) = self.infer_expr(item);
                    typed_items.push(item);
                    types.push(type_);
                }
                for i in 0..(types.len() - 1) {
                    let a = &types[i];
                    let b = &types[i + 1];
                    let a_span = typed_items[i].span;
                    let b_span = typed_items[i + 1].span;
                    let source = ConstraintSource::ListItems(
                        i,
                        a_span,
                        b_span
                    );
                    self.add_constraint(a, b, source);
                }
                let type_var = self.fresh_var();
                if let Some(ref t) = types.get(0) {
                    self.add_constraint(
                        t,
                        &Type::Var(type_var),
                        ConstraintSource::AlwaysStatisfied
                    );
                }
                let type_ = types.pop().unwrap_or_else(|| Type::Var(self.fresh_var()));
                let list = Type::Concrete("List.List".to_string());
                let type_ = Type::Apply(Rc::new(list), Rc::new(type_));
                (make_list(typed_items, type_var), type_)
            }
            r::Expr::Literal(ref literal) => {
                let type_ = self.infer_literal(literal);
                (t::Expr::Literal(literal.clone(), type_.clone()), type_)
            }
            r::Expr::Parenthesised(ref expr) => {
                return self.infer_expr(expr);
            }
            r::Expr::Tuple(ref items) => {
                let mut typed_items = Vec::new();
                let mut types = Vec::new();
                for item in items {
                    let (item, type_) = self.infer_expr(item);
                    typed_items.push(item);
                    types.push(type_);
                }
                (t::Expr::Tuple(typed_items), Type::Tuple(types))
            }
        };
        (Node::new(typed, expr.span), type_)
    }

    fn infer_defs(&mut self, defs: &[r::Def]) {
        
    }
}

fn make_list(mut items: Vec<Node<t::Expr>>, _type_var: u64) -> t::Expr {
    let nil_sym = t::Symbol::Global("List.Nil".to_string());
    let nil = Node::new(t::Expr::Var(nil_sym, Type::Any), DUMMY_SPAN);
    // TODO: figure out if exported name is really like this
    let cons_sym = t::Symbol::Global("List.(::)".to_string());
    let cons = Node::new(t::Expr::Var(cons_sym, Type::Any), DUMMY_SPAN);
    let mut list = nil;
    while let Some(item) = items.pop() {
        let span = item.span;
        let cons = t::Expr::Apply(Box::new(cons.clone()), Box::new(item));
        let cons = Node::new(cons, span);
        let cons = t::Expr::Apply(Box::new(cons), Box::new(list));
        list = Node::new(cons, DUMMY_SPAN);
    }
    list.value
}

fn make_list_pattern(mut items: Vec<Node<t::Pattern>>, _type_var: u64) -> t::Pattern {
    let nil_sym = Node::new("List.Nil".to_string(), DUMMY_SPAN);
    let nil = Node::new(t::Pattern::Deconstruct(nil_sym, Vec::new()), DUMMY_SPAN);
    // TODO: figure out if exported name is really like this
    let cons_sym = Node::new("List.(::)".to_string(), DUMMY_SPAN);
    let mut list = nil;
    while let Some(item) = items.pop() {
        let span = item.span;
        let cons = t::Pattern::Deconstruct(cons_sym.clone(), vec![item, list]);
        list = Node::new(cons, DUMMY_SPAN);
    }
    list.value
}

fn make_if(cond: Node<t::Expr>, then: Node<t::Expr>, else_: Node<t::Expr>) -> t::Expr {
    let then_span = then.span;
    let else_span = else_.span;
    let bool_type = Type::Concrete("Basics.Bool".to_string());
    let true_ = t::Pattern::Literal(Literal::Bool(true), bool_type.clone());
    let true_ = t::CaseBranch {
        pattern: Node::new(true_, DUMMY_SPAN),
        guard: None,
        value: then,
    };
    let false_ = t::Pattern::Literal(Literal::Bool(false), bool_type.clone());
    let false_ = t::CaseBranch {
        pattern: Node::new(false_, DUMMY_SPAN),
        guard: None,
        value: else_,
    };
    t::Expr::Case(Box::new(cond), vec![
        Node::new(true_, then_span),
        Node::new(false_, else_span),
    ])
}

fn function_name(expr: &r::Expr) -> (String, usize) {
    match *expr {
        r::Expr::Apply(ref expr, _) => {
            let (name, pos) = function_name(&expr.value);
            (name, pos + 1)
        }
        r::Expr::Parenthesised(ref expr) |
        r::Expr::Let(_, _, ref expr) => {
            function_name(&expr.value)
        }
        r::Expr::Ident(ref s) => {
            (s.clone().full_name(), 0)
        }
        r::Expr::Infix(_, ref op, _) => {
            (op.value.clone().full_name(), 0)
        }
        r::Expr::Lambda(_, _) => {
            ("lambda function".to_string(), 0)
        }
        _ => {
            ("function".to_string(), 0)
        }
    }
}

fn empty_scheme(type_: Type) -> Scheme {
    Scheme {
        vars: Vec::new(),
        type_: type_,
    }
}
