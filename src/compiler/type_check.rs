use std::collections::{BTreeSet, BTreeMap};
use std::mem;
use std::rc::Rc;
use codemap::Span;
use ast::{Node, Literal, Symbol, Sym};
use ast::resolved as r;
use ast::typed::{self as t, Type, Scheme, SchemeVar, ImplSym, ImplDef, Impls};
use compiler::{builtins, util};
use symbols::SymbolSource;
use CompileCtx;


#[derive(Debug, Copy, Clone)]
enum FunctionName {
    Sym(Sym),
    Str(&'static str),
}

impl FunctionName {
    fn from_symbol(symbol: Symbol) -> Self {
        match symbol {
            Symbol::Known(sym) => FunctionName::Sym(sym),
            Symbol::Unknown => FunctionName::Str("Function"),
        }
    }

    fn name<'a>(self, symbols: &'a SymbolSource) -> &'a str {
        match self {
            FunctionName::Sym(sym) => symbols.symbol_name(sym),
            FunctionName::Str(name) => name,
        }
    }
}

#[derive(Clone)]
struct PatternTy {
    vars: Vec<u64>,
    whole_type: Type,
    items: Vec<Type>,
}

#[derive(Debug)]
enum ConstraintSource {
    AlwaysStatisfied,
    IfCondition(Span),
    IfBranches(Span, Span),
    MatchBranches(usize, Span, Span),
    MatchedValue(Span),
    ListItems(usize, Span, Span),
    InfixOperator(Span),
    FunctionArg(usize, FunctionName, Span),
    Function(Span),
    DoIf(Span),
    BindSource(Span),
    BindRest(Span),
    Annotated(Sym, Span, Span),
    Infered(Sym, Span),
}

#[derive(Debug)]
struct Constraint(Type, Type, ConstraintSource);

struct InferCtx<'a> {
    pattern_types: &'a BTreeMap<Sym, PatternTy>,
    env: BTreeMap<Sym, Scheme>,
    new_env: BTreeMap<Sym, Scheme>,
    next_var: u64,
    var_bounds: BTreeMap<u64, BTreeSet<Sym>>,
    ctx: &'a mut CompileCtx,
    constraints: Vec<Constraint>,
    annotations: &'a BTreeMap<Sym, (Scheme, Span)>,
}

impl<'a> InferCtx<'a> {
    fn new(
        pattern_types: &'a BTreeMap<Sym, PatternTy>,
        annotations: &'a BTreeMap<Sym, (Scheme, Span)>,
        ctx: &'a mut CompileCtx,
    ) -> Self {
        InferCtx {
            pattern_types,
            env: BTreeMap::new(),
            new_env: BTreeMap::new(),
            next_var: 1,
            var_bounds: BTreeMap::new(),
            ctx,
            constraints: Vec::new(),
            annotations,
        }
    }

    fn fresh_var(&mut self) -> u64 {
        let var = self.next_var;
        self.next_var += 1;
        var
    }

    fn fresh_sym(&mut self) -> Sym {
        self.ctx.symbols.fresh_artificial_sym()
    }

    fn add_var_bound(&mut self, var: u64, trait_: Sym) {
        self.var_bounds
            .entry(var)
            .or_insert_with(BTreeSet::new)
            .insert(trait_);
    }

    fn add_constraint(&mut self, a: &Type, b: &Type, source: ConstraintSource) {
        let constraint = Constraint(
            a.clone(),
            b.clone(),
            source,
        );
        self.constraints.push(constraint);
    }

    fn add_symbol(&mut self, symbol: Sym, type_: Scheme) {
        if !self.annotations.contains_key(&symbol) {
            self.new_env.insert(symbol, type_);
        }
    }

    fn instantiate_scheme(&mut self, scheme: &Scheme) -> Type {
        let mut substitution = BTreeMap::new();
        for var in &scheme.vars {
            let to = self.fresh_var();
            for &bound in &var.bounds {
                self.add_var_bound(to, bound);
            }
            substitution.insert(var.id, Type::Var(to));
        }
        scheme.type_.map_vars(&substitution)
    }

    fn instantiate_pattern_ty(&mut self, pattern: Sym) -> (Type, Vec<Type>) {
        let ty = self.pattern_types[&pattern].clone();
        let mut substitution = BTreeMap::new();
        for &var in &ty.vars {
            substitution.insert(var, Type::Var(self.fresh_var()));
        }
        let whole_type = ty.whole_type.map_vars(&substitution);
        let mut items = Vec::new();
        for item in &ty.items {
            let item = item.map_vars(&substitution);
            items.push(item);
        }
        (whole_type, items)
    }

    fn free_env_vars(&self) -> BTreeSet<u64> {
        let mut vars = BTreeSet::new();
        for scheme in self.new_env.values() {
            scheme.type_.collect_vars(&mut vars);
            for var in &scheme.vars {
                vars.remove(&var.id);
            }
        }
        vars
    }

    fn infer_literal(&mut self, literal: &Literal) -> Type {
        match *literal {
            Literal::Bool(_) => Type::Concrete(builtins::types::BOOL),
            Literal::Char(_) => Type::Concrete(builtins::types::CHAR),
            Literal::Float(_) => Type::Concrete(builtins::types::FRAC),
            Literal::Str(_) => Type::Concrete(builtins::types::STRING),
            Literal::Int(_) => Type::Concrete(builtins::types::INT),
        }
    }

    fn lookup_env(&mut self, sym: Sym) -> Scheme {
        if let Some(s) = self.annotations.get(&sym).cloned() {
            s.0
        } else if let Some(s) = self.env.get(&sym).cloned() {
            s
        } else if let Some(s) = self.new_env.get(&sym).cloned() {
            s
        } else {
            panic!("symbol `{}` ({:?}) is not in env",
                self.ctx.symbols.symbol_name(sym),
                sym);
        }
    }

    fn infer_symbol(&mut self, symbol: Symbol) -> Type {
        match symbol {
            Symbol::Unknown => {
                return Type::Any;
            }
            Symbol::Known(sym) => {
                let scheme = self.lookup_env(sym);
                self.instantiate_scheme(&scheme)
            }
        }
    }

    fn infer_pattern(&mut self, pattern: &Node<r::Pattern>) -> (Node<t::Pattern>, Type) {
        let (typed, type_) = match pattern.value {
            r::Pattern::As(ref pat, ref name) => {
                let (pat, type_) = self.infer_pattern(&**pat);
                self.add_symbol(name.value, empty_scheme(type_.clone()));
                (t::Pattern::As(Box::new(pat), name.clone()), type_)
            }
            r::Pattern::Deconstruct(ref name, ref args) => {
                match name.value {
                    Symbol::Known(sym) => {
                        let (ty, args_types) = self.instantiate_pattern_ty(sym);
                        let mut typed_args = Vec::new();
                        for (i, arg) in args.iter().enumerate() {
                            let expected = args_types.get(i).unwrap_or(&Type::Any);
                            let (arg, type_) = self.infer_pattern(arg);
                            let source = ConstraintSource::MatchedValue(arg.span);
                            self.add_constraint(&type_, expected, source);
                            typed_args.push(arg);
                        }
                        if args.len() != args_types.len() {
                            // TODO: maybe extract this into its own pass?
                            let msg = format!(
                                "Pattern `{}` takes {} arguments, but here it has {}.",
                                self.ctx.symbols.symbol_name(sym),
                                args_types.len(),
                                args.len());
                            self.ctx.reporter
                                .pattern_error(msg.as_str(), pattern.span)
                                .span_note(msg, pattern.span)
                                .done();
                            (t::Pattern::Wildcard, Type::Any)
                        } else {
                            let pat = t::Pattern::Deconstruct(name.clone(), typed_args);
                            (pat, ty)
                        }
                    }
                    Symbol::Unknown => {
                        (t::Pattern::Wildcard, Type::Any)
                    }
                }
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
                for i in 0..(types.len().saturating_sub(1)) {
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
                let list = Type::Concrete(builtins::types::LIST);
                let type_ = Type::Apply(Rc::new(list), Rc::new(type_));
                (make_list_pattern(pattern.span, typed_items), type_)
            }
            r::Pattern::Literal(ref literal) => {
                let type_ = self.infer_literal(literal);
                (t::Pattern::Literal(literal.clone()), type_)
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
            r::Pattern::Wildcard => {
                let type_ = Type::Var(self.fresh_var());
                (t::Pattern::Wildcard, type_)
            }
        };
        (Node::new(typed, pattern.span), type_)
    }

    fn infer_expr(&mut self, expr: &Node<r::Expr>) -> (Node<t::Expr>, Type) {
        let (typed, type_) = match expr.value {
            r::Expr::Apply(ref a, ref b) => {
                let (fn_name, arg_pos) = function_name(&a.value);
                let (a, atype) = self.infer_expr(&**a);
                let (b, btype) = self.infer_expr(&**b);
                let var1 = Type::Var(self.fresh_var());
                let var2 = Type::Var(self.fresh_var());
                let fn_type = Type::Function(Rc::new(var1.clone()), Rc::new(var2.clone()));
                let source = ConstraintSource::Function(a.span);
                self.add_constraint(&atype, &fn_type, source);
                let source = ConstraintSource::FunctionArg(arg_pos, fn_name, b.span);
                self.add_constraint(&var1, &btype, source);
                let expr = t::Expr::Apply(Box::new(a), Box::new(b));
                (expr, var2)
            }
            r::Expr::Case(ref value, ref branches) => {
                let (value, type_) = self.infer_expr(&**value);
                let mut typed_branches = Vec::new();
                let mut last_branch = None;
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
                        let source = ConstraintSource::IfCondition(expr.span);
                        let bool_type = Type::Concrete(builtins::types::BOOL);
                        self.add_constraint(&type_, &bool_type, source);
                        typed
                    });
                    last_branch = Some((val_type, branch.value.span));
                    typed_branches.push(Node::new(t::CaseBranch {
                        pattern: pat,
                        guard: guard,
                        value: val,
                    }, span));
                }
                let expr = t::Expr::Case(Box::new(value), typed_branches);
                (expr, last_branch.unwrap().0)
            }
            r::Expr::Ident(symbol) => {
                let type_ = self.infer_symbol(symbol);
                (t::Expr::Var(symbol, type_.clone(), Impls::empty()), type_)
            }
            r::Expr::If(ref cond, ref then, ref else_) => {
                let (cond, cond_type) = self.infer_expr(cond);
                let (then, then_type) = self.infer_expr(then);
                let (else_, else_type) = self.infer_expr(else_);
                let bool_type = Type::Concrete(builtins::types::BOOL);
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
                let expected_op_type = Type::Function(
                    Rc::new(Type::Var(left_var)),
                    Rc::new(Type::Function(
                        Rc::new(Type::Var(right_var)),
                        Rc::new(Type::Var(out_var))
                    ))
                );
                let op_type = self.infer_symbol(op.value);
                let (typed_lhs, ltype) = self.infer_expr(lhs);
                let (typed_rhs, rtype) = self.infer_expr(rhs);
                let source = ConstraintSource::InfixOperator(op.span);
                self.add_constraint(&op_type, &expected_op_type, source);
                let source = ConstraintSource::FunctionArg(
                    0,
                    FunctionName::from_symbol(op.value),
                    op.span
                );
                self.add_constraint(&Type::Var(left_var), &ltype, source);
                let source = ConstraintSource::FunctionArg(
                    1,
                    FunctionName::from_symbol(op.value),
                    op.span
                );
                self.add_constraint(&Type::Var(right_var), &rtype, source);
                let expr = if op.value == Symbol::Known(builtins::values::AND) {
                    make_and(typed_lhs, typed_rhs, op.span)
                } else if op.value == Symbol::Known(builtins::values::OR) {
                    make_or(typed_lhs, typed_rhs, op.span)
                } else {
                    let op_expr = t::Expr::Var(op.value, op_type, Impls::empty());
                    let op = Node::new(op_expr, op.span);
                    let span = lhs.span.merge(op.span);
                    let apply_lhs = t::Expr::Apply(Box::new(op), Box::new(typed_lhs));
                    let first = Node::new(apply_lhs, span);
                    t::Expr::Apply(Box::new(first), Box::new(typed_rhs))
                };
                let type_ = Type::Var(out_var);
                (expr, type_)
            }
            r::Expr::Lambda(ref param, ref value) => {
                let param_type = Type::Var(self.fresh_var());
                self.add_symbol(param.value, empty_scheme(param_type.clone()));
                let (value, body_type) = self.infer_expr(value);
                let fn_type = Type::Function(Rc::new(param_type), Rc::new(body_type));
                let lambda = t::Expr::Lambda(param.clone(), Box::new(value));
                (lambda, fn_type)
            }
            r::Expr::Let(ref defs, ref value) => {
                let mut typed_defs = self.infer_defs(defs, true);
                // infer typed ones after untyped
                for def in defs {
                    if self.annotations.contains_key(&def.sym.value) {
                        let slice = util::slice_from_ref(def);
                        typed_defs.extend(self.infer_defs(slice, false));
                    }
                }
                debug_assert_eq!(defs.len(), typed_defs.len());
                let (value, type_) = self.infer_expr(value);
                let expr = t::Expr::Let(typed_defs, Box::new(value));
                (expr, type_)
            }
            r::Expr::List(ref items) => {
                let mut typed_items = Vec::new();
                let mut types = Vec::new();
                for item in items {
                    let (item, type_) = self.infer_expr(item);
                    typed_items.push(item);
                    types.push(type_);
                }
                // TODO: this might be nices if rewritten to use `windows`
                for i in 0..(types.len().saturating_sub(1)) {
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
                let list = Type::Concrete(builtins::types::LIST);
                let type_ = Type::Apply(Rc::new(list), Rc::new(type_));
                (make_list(expr.span, typed_items, type_var), type_)
            }
            r::Expr::Literal(ref literal) => {
                let type_ = self.infer_literal(literal);
                (t::Expr::Literal(literal.clone()), type_)
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
            r::Expr::DoIf(ref cond, ref rest) => {
                let (cond, cond_type) = self.infer_expr(cond);
                let (rest, rest_type) = self.infer_expr(rest);
                // expect `cond : Bool`
                let bool_type = Type::Concrete(builtins::types::BOOL);
                let cond_source = ConstraintSource::IfCondition(cond.span);
                self.add_constraint(&cond_type, &bool_type, cond_source);
                // expect `rest : m a`, where `m : Failable`
                let m = self.fresh_var();
                let a = self.fresh_var();
                self.add_var_bound(m, builtins::traits::FAILABLE);
                let expected = Type::Apply(Rc::new(Type::Var(m)), Rc::new(Type::Var(a)));
                let source = ConstraintSource::DoIf(rest.span);
                self.add_constraint(&rest_type, &expected, source);
                // make `if cond then rest else fail`
                let fail = Symbol::Known(builtins::values::FAIL);
                let fail = t::Expr::Var(fail, expected, Impls::empty());
                let fail = Node::new(fail, cond.span);
                let if_ = make_if(cond, rest, fail);
                (if_, rest_type)
            }
            r::Expr::Bind(ref pat, ref expr, ref rest) => {
                let m = self.fresh_var();
                let matched = self.fresh_var();
                self.add_var_bound(m, builtins::traits::COMPUTATION);
                let m = Rc::new(Type::Var(m));
                let a = Rc::new(Type::Var(matched));
                let (pat, pat_type) = self.infer_pattern(pat);
                let (expr, expr_type) = self.infer_expr(expr);
                let (rest, rest_type) = self.infer_expr(rest);
                let pat_type_rc = Rc::new(pat_type.clone());
                // expect `expr : m a`
                let expr_expected = Type::Apply(m.clone(), a.clone());
                let source = ConstraintSource::BindSource(expr.span);
                self.add_constraint(&expr_type, &expr_expected, source);
                // expect `pat : a`
                let source = ConstraintSource::MatchedValue(pat.span);
                self.add_constraint(&pat_type, &a, source);
                // expect `rest : m b`
                let var = Type::Var(self.fresh_var());
                let rest_expected = Type::Apply(m.clone(), Rc::new(var));
                let source = ConstraintSource::BindRest(rest.span);
                self.add_constraint(&rest_type, &rest_expected, source);
                // build case expr for pattern
                let pat_span = pat.span;
                let param_sym = self.fresh_sym();
                self.add_symbol(param_sym, empty_scheme((*a).clone()));
                let param = t::Expr::Var(Symbol::Known(param_sym), pat_type, Impls::empty());
                let param = Node::new(param, pat.span);
                let span = pat.span.merge(rest.span);
                let branch = t::CaseBranch {
                    pattern: pat,
                    guard: None,
                    value: rest,
                };
                let branch = Node::new(branch, span);
                let span = branch.span;
                let case = t::Expr::Case(Box::new(param), vec![branch]);
                let case = Node::new(case, span);
                // build lambda from case and rest
                let span = case.span;
                let param = Node::new(param_sym, pat_span);
                let lambda = t::Expr::Lambda(param, Box::new(case));
                let lambda = Node::new(lambda, span);
                // apply expr and lambda to bind
                let ma = Rc::new(expr_expected);
                let mb = Rc::new(rest_expected.clone());
                let a = pat_type_rc;
                let bind_type =
                    Type::Function(
                        Rc::new(Type::Function(a, mb.clone())),
                        Rc::new(Type::Function(ma, mb)));
                let bind_symbol = Symbol::Known(builtins::values::AND_THEN);
                let bind = t::Expr::Var(bind_symbol, bind_type, Impls::empty());
                let bind = Node::new(bind, expr.span);
                let span = expr.span;
                let result = t::Expr::Apply(Box::new(bind), Box::new(lambda));
                let result = Node::new(result, span);
                let result = t::Expr::Apply(Box::new(result), Box::new(expr));
                (result, rest_expected)
            }
        };
        (Node::new(typed, expr.span), type_)
    }

    fn merge_var_bounds(&mut self, into: u64, from: u64) {
        if into == from {
            return;
        }
        let bounds = match self.var_bounds.get(&from) {
            Some(bounds) => bounds.clone(),
            None => return,
        };
        self.var_bounds
            .entry(into)
            .or_insert_with(BTreeSet::new)
            .extend(bounds);
    }

    fn infer_defs(&mut self, defs: &[r::Def], skip_typed: bool) -> Vec<t::Def> {
        let free_vars = self.free_env_vars();
        for def in defs {
            let type_ = empty_scheme(Type::Var(self.fresh_var()));
            self.add_symbol(def.sym.value, type_);
        }
        let mut result = Vec::new();
        let mut constraints = Vec::new();
        mem::swap(&mut self.constraints, &mut constraints);
        for def in defs {
            if skip_typed && self.annotations.contains_key(&def.sym.value) {
                continue;
            }
            let (body, type_) = self.infer_expr(&def.value);
            let annot = self.annotations.get(&def.sym.value);
            if let Some(&(ref scheme, scheme_span)) = annot {
                let annotated = self.instantiate_scheme(scheme);
                let source = ConstraintSource::Annotated(def.sym.value, def.sym.span, scheme_span);
                self.add_constraint(&type_, &annotated, source);
            } else {
                let given_type = self.lookup_env(def.sym.value).type_;
                let source = ConstraintSource::Infered(
                    def.sym.value,
                    def.sym.span);
                self.add_constraint(&type_, &given_type, source);
            }
            let typed = t::Def {
                sym: def.sym.clone(),
                value: body,
                scheme: Scheme { vars: Vec::new(), type_ },
            };
            result.push(typed);
        }
        // solve constraints, apply substitution
        mem::swap(&mut self.constraints, &mut constraints);
        let (substitution, var_unifications) = {
            let solver = Solver::new(
                &constraints,
                self.ctx);
            solver.solve_constraints()
        };
        self.constraints.extend(constraints);
        for (a, b) in var_unifications {
            self.merge_var_bounds(a, b);
            self.merge_var_bounds(b, a);
        }
        for val in self.new_env.values_mut() {
            val.type_ = val.type_.map_vars(&substitution);
        }
        // get free variables after substitution
        let free_vars = {
            let mut result = BTreeSet::new();
            for &var in &free_vars {
                if let Some(type_) = substitution.get(&var) {
                    type_.collect_vars(&mut result);
                } else {
                    result.insert(var);
                }
            }
            result
        };
        // generalize def types
        for def in &mut result {
            let mut scheme = def.scheme.clone();
            scheme.type_ = scheme.type_.map_vars(&substitution);
            let mut vars = BTreeSet::new();
            scheme.type_.collect_vars(&mut vars);
            let mut scheme_vars = Vec::new();
            for &v in &vars {
                if !free_vars.contains(&v) {
                    let v = SchemeVar {
                        id: v,
                        bounds: self.var_bounds
                            .get(&v)
                            .cloned()
                            .unwrap_or_else(BTreeSet::new)
                            .into_iter()
                            .collect(),
                    };
                    scheme_vars.push(v);
                }
            }
            let scheme = Scheme {
                vars: scheme_vars,
                .. scheme
            };
            self.new_env.insert(def.sym.value, scheme.clone());
            def.scheme = scheme;
            def.value.value.substitute_inner(&substitution);
        }

        result
    }

    fn infer_top_level_defs(&mut self, defs: &[r::Def]) -> Vec<t::Def> {
        debug_assert!(self.new_env.is_empty());
        let mut typed_defs = self.infer_defs(&defs, true);
        // infer typed ones after untyped
        for def in defs {
            if self.annotations.contains_key(&def.sym.value) {
                let slice = util::slice_from_ref(def);
                typed_defs.extend(self.infer_defs(slice, false));
            }
        }
        debug_assert_eq!(defs.len(), typed_defs.len());
        for (k, v) in ::std::mem::replace(&mut self.new_env, BTreeMap::new()) {
            if !self.env.contains_key(&k) {
                self.env.insert(k, v);
            }
        }
        typed_defs
    }
}

fn make_list(span: Span, mut items: Vec<Node<t::Expr>>, type_var: u64) -> t::Expr {
    let list_type = Rc::new(Type::Apply(
        Rc::new(Type::Concrete(builtins::types::LIST)),
        Rc::new(Type::Var(type_var))));
    let nil_span = items.last().map(|i| i.span).unwrap_or(span);
    let nil_sym = t::Expr::Var(
        Symbol::Known(builtins::values::NIL),
        (*list_type).clone(),
        Impls::empty());
    let nil = Node::new(nil_sym, nil_span);
    let cons_type = Type::Function(
        Rc::new(Type::Var(type_var)),
        Rc::new(Type::Function(list_type.clone(), list_type.clone())));
    let cons_sym = t::Expr::Var(
        Symbol::Known(builtins::values::CONS),
        cons_type,
        Impls::empty());
    let mut list = nil;
    while let Some(item) = items.pop() {
        let first_span = item.span;
        let span = item.span.merge(list.span);
        let cons = Node::new(cons_sym.clone(), item.span);
        let cons = t::Expr::Apply(Box::new(cons.clone()), Box::new(item));
        let cons = Node::new(cons, first_span);
        let cons = t::Expr::Apply(Box::new(cons), Box::new(list));
        list = Node::new(cons, span);
    }
    list.value
}

fn make_list_pattern(span: Span, mut items: Vec<Node<t::Pattern>>) -> t::Pattern {
    let nil_span = items.last().map(|i| i.span).unwrap_or(span);
    let nil_sym = Node::new(Symbol::Known(builtins::values::NIL), nil_span);
    let nil = Node::new(t::Pattern::Deconstruct(nil_sym, Vec::new()), nil_span);
    let mut list = nil;
    while let Some(item) = items.pop() {
        let cons_sym = Node::new(Symbol::Known(builtins::values::CONS), item.span);
        let span = item.span.merge(list.span);
        let cons = t::Pattern::Deconstruct(cons_sym, vec![item, list]);
        list = Node::new(cons, span);
    }
    list.value
}

fn make_if(cond: Node<t::Expr>, then: Node<t::Expr>, else_: Node<t::Expr>) -> t::Expr {
    let then_span = then.span;
    let else_span = else_.span;
    let true_ = t::Pattern::Literal(Literal::Bool(true));
    let true_ = t::CaseBranch {
        pattern: Node::new(true_, then.span),
        guard: None,
        value: then,
    };
    let false_ = t::Pattern::Literal(Literal::Bool(false));
    let false_ = t::CaseBranch {
        pattern: Node::new(false_, else_.span),
        guard: None,
        value: else_,
    };
    t::Expr::Case(Box::new(cond), vec![
        Node::new(true_, then_span),
        Node::new(false_, else_span),
    ])
}

fn make_and(lhs: Node<t::Expr>, rhs: Node<t::Expr>, op_span: Span) -> t::Expr {
    let f = Node::new(t::Expr::Literal(Literal::Bool(false)), op_span);
    make_if(lhs, rhs, f)
}

fn make_or(lhs: Node<t::Expr>, rhs: Node<t::Expr>, op_span: Span) -> t::Expr {
    let t = Node::new(t::Expr::Literal(Literal::Bool(true)), op_span);
    make_if(lhs, t, rhs)
}

fn function_name(expr: &r::Expr) -> (FunctionName, usize) {
    match *expr {
        r::Expr::Apply(ref expr, _) => {
            let (name, pos) = function_name(&expr.value);
            (name, pos + 1)
        }
        r::Expr::Parenthesised(ref expr) |
        r::Expr::Let(_, ref expr) => {
            function_name(&expr.value)
        }
        r::Expr::Ident(Symbol::Known(s)) => {
            (FunctionName::Sym(s), 0)
        }
        r::Expr::Infix(_, ref op, _) => {
            (FunctionName::from_symbol(op.value), 0)
        }
        r::Expr::Lambda(_, _) => {
            (FunctionName::Str("Lambda function"), 0)
        }
        _ => {
            (FunctionName::Str("Function"), 0)
        }
    }
}

fn empty_scheme(type_: Type) -> Scheme {
    Scheme {
        vars: Vec::new(),
        type_: type_,
    }
}

struct Solver<'a> {
    constraints: &'a [Constraint],
    ctx: &'a mut CompileCtx,
    substitution: BTreeMap<u64, Type>,
    var_unifications: Vec<(u64, u64)>,
}

impl<'a> Solver<'a> {
    fn new(
        constraints: &'a [Constraint],
        ctx: &'a mut CompileCtx,
    ) -> Self {
        Solver {
            constraints,
            ctx,
            substitution: BTreeMap::new(),
            var_unifications: Vec::new(),
        }
    }

    fn do_substitutions(&self, type_: &Type) -> Type {
        match *type_ {
            Type::Any => Type::Any,
            Type::Apply(ref a, ref b) => {
                let a = Rc::new(self.do_substitutions(a));
                let b = Rc::new(self.do_substitutions(b));
                Type::Apply(a, b)
            }
            Type::Concrete(sym) => Type::Concrete(sym),
            Type::Function(ref a, ref b) => {
                let a = Rc::new(self.do_substitutions(a));
                let b = Rc::new(self.do_substitutions(b));
                Type::Function(a, b)
            }
            Type::Tuple(ref types) => {
                let types = types.iter().map(|t| self.do_substitutions(t)).collect();
                Type::Tuple(types)
            }
            Type::Var(var) => {
                self.substitution.get(&var).cloned().unwrap_or(Type::Var(var))
            }
        }
    }

    fn add_mapping(&mut self, var: u64, type_: Type) {
        for v in self.substitution.values_mut() {
            *v = substitute_var(v, var, &type_);
        }
        self.substitution.insert(var, type_);
    }

    fn unify<'d>(&mut self, a: &'d Type, b: &'d Type) -> bool {
        match (a, b) {
            (&Type::Any, _) | (_, &Type::Any) => true,
            (&Type::Var(v1), &Type::Var(v2)) => {
                if v1 != v2 {
                    self.var_unifications.push((v1, v2));
                    self.add_mapping(v1, Type::Var(v2));
                }
                true
            }
            (&Type::Var(var), typ) | (typ, &Type::Var(var)) => {
                if typ.contains_var(var) {
                    false
                } else {
                    self.add_mapping(var, typ.clone());
                    true
                }
            }
            (&Type::Tuple(ref a), &Type::Tuple(ref b)) => {
                if a.len() != b.len() {
                    false
                } else {
                    a.iter().zip(b.iter()).all(|(a, b)| self.unify(a, b))
                }
            }
            (&Type::Function(ref a1, ref a2), &Type::Function(ref b1, ref b2)) |
            (&Type::Apply(ref a1, ref a2), &Type::Apply(ref b1, ref b2)) => {
                self.unify(a1, b1) && {
                    let a2 = self.do_substitutions(a2);
                    let b2 = self.do_substitutions(b2);
                    self.unify(&a2, &b2)
                }
            }
            (&Type::Concrete(a), &Type::Concrete(b)) => {
                a == b
            }
            _ => {
                false
            }
        }
    }

    fn unsolved_constraint(&mut self, constraint: &Constraint) {
        let type1 = self.do_substitutions(&constraint.0);
        let type1 = type1.display(&self.ctx.symbols);
        let type2 = self.do_substitutions(&constraint.1);
        let type2 = type2.display(&self.ctx.symbols);
        match constraint.2 {
            ConstraintSource::AlwaysStatisfied => {
                panic!("failed to solve constraint \
                        that should not have failed");
            }
            ConstraintSource::DoIf(span) => {
                let msg = format!(
                    "Remaining statements have type `{}`, \
                    while it should be `m a`.",
                    type1);
                self.ctx.reporter
                    .type_error(msg.as_str(), span)
                    .span_note(msg, span)
                    .done();
            }
            ConstraintSource::Annotated(sym, def_span, annot_span) => {
                let msg1 = format!(
                    "Infered type `{}` for `{}`.",
                    type1,
                    self.ctx.symbols.symbol_name(sym));
                let msg2 = format!(
                    "but annotation says it should have type `{}`.",
                    type2);
                self.ctx.reporter
                    .type_error(msg1.as_str(), def_span)
                    .span_note(msg1, def_span)
                    .span_note(msg2, annot_span)
                    .done();
            }
            ConstraintSource::BindRest(span) => {
                let msg = format!(
                    "Remaining statements have type `{}`, \
                    while expected type was `{}`.",
                    type1,
                    type2);
                self.ctx.reporter
                    .type_error(msg.as_str(), span)
                    .span_note(msg, span)
                    .done();
            }
            ConstraintSource::BindSource(span) => {
                let msg = format!(
                    "Value that is being unwrapped has type `{}`, while it \
                    should be `m a`",
                    type1);
                self.ctx.reporter
                    .type_error(msg.as_str(), span)
                    .span_note(msg, span)
                    .done();
            }
            ConstraintSource::Function(span) => {
                let msg1 = format!(
                    "This expression has type `{}`, and cannot be used as a function.",
                    type1);
                self.ctx.reporter
                    .type_error(msg1.as_str(), span)
                    .span_note(msg1, span)
                    .done();
            }
            ConstraintSource::FunctionArg(index, fn_name, span) => {
                let msg = format!(
                    "`{}` expects {}{} arg to have type `{}`, \
                    but it has type `{}`.",
                    fn_name.name(&self.ctx.symbols),
                    index + 1,
                    number_suffix(index + 1),
                    type1,
                    type2);
                self.ctx.reporter
                    .type_error(msg.as_str(), span)
                    .span_note(msg, span)
                    .done();
            }
            ConstraintSource::IfBranches(first_span, second_span) => {
                let msg1 = format!(
                    "If branches have different types, then branch has type `{}`,",
                    type1);
                let msg2 = format!(
                    "while else branch has type `{}`,",
                    type2);
                self.ctx.reporter
                    .type_error(msg1.as_str(), first_span)
                    .span_note(msg1, first_span)
                    .span_note(msg2, second_span)
                    .done();
            }
            ConstraintSource::IfCondition(span) => {
                let msg = format!(
                    "Condition has type `{}`, when it should be `Bool`.",
                    type1);
                self.ctx.reporter
                    .type_error(msg.as_str(), span)
                    .span_note(msg, span)
                    .done();
            }
            ConstraintSource::InfixOperator(span) => {
                let msg = format!(
                    "Infix operator has type `{}`, when it should be a \
                    function with two parameters.",
                    type1);
                self.ctx.reporter
                    .type_error(msg.as_str(), span)
                    .span_note(msg, span)
                    .done();
            }
            ConstraintSource::ListItems(first_index, first_span, second_span) => {
                let msg1 = format!(
                    "List items have different types, {}{} item has type `{}`,",
                    first_index + 1,
                    number_suffix(first_index + 1),
                    type1);
                let msg2 = format!(
                    "while {}{} item has type `{}`,",
                    first_index + 2,
                    number_suffix(first_index + 2),
                    type2);
                self.ctx.reporter
                    .type_error(msg1.as_str(), first_span)
                    .span_note(msg1, first_span)
                    .span_note(msg2, second_span)
                    .done();
            }
            ConstraintSource::MatchBranches(first_index, first_span, second_span) => {
                let msg1 = format!(
                    "Case branches have different types, {}{} branch has type `{}`,",
                    first_index + 1,
                    number_suffix(first_index + 1),
                    type1);
                let msg2 = format!(
                    "while {}{} branch has type `{}`,",
                    first_index + 2,
                    number_suffix(first_index + 2),
                    type2);
                self.ctx.reporter
                    .type_error(msg1.as_str(), first_span)
                    .span_note(msg1, first_span)
                    .span_note(msg2, second_span)
                    .done();
            }
            ConstraintSource::MatchedValue(span) => {
                let msg = format!(
                    "This pattern matches values of type `{}`, \
                    while matched value has type `{}`.",
                    type2,
                    type1);
                self.ctx.reporter
                    .type_error(msg.as_str(), span)
                    .span_note(msg, span)
                    .done();
            }
            ConstraintSource::Infered(sym, span) => {
                let msg = format!(
                    "Infered type `{}` for `{}` from definition body, \
                    but it seems to be `{}` based on usage.",
                    type1,
                    self.ctx.symbols.symbol_name(sym),
                    type2);
                self.ctx.reporter
                    .type_error(msg.as_str(), span)
                    .span_note(msg, span)
                    .done();
            }
        }
    }

    fn solve_constraints(mut self) -> (BTreeMap<u64, Type>, Vec<(u64, u64)>) {
        for constraint in self.constraints {
            let type1 = self.do_substitutions(&constraint.0);
            let type2 = self.do_substitutions(&constraint.1);
            if !self.unify(&type1, &type2) {
                self.unsolved_constraint(constraint);
            }
        }
        (self.substitution, self.var_unifications)
    }
}

fn substitute_var(type_: &Type, var: u64, new_type: &Type) -> Type {
    match *type_ {
        Type::Any => Type::Any,
        Type::Apply(ref a, ref b) => {
            let a = Rc::new(substitute_var(a, var, new_type));
            let b = Rc::new(substitute_var(b, var, new_type));
            Type::Apply(a, b)
        }
        Type::Concrete(sym) => Type::Concrete(sym),
        Type::Function(ref a, ref b) => {
            let a = Rc::new(substitute_var(a, var, new_type));
            let b = Rc::new(substitute_var(b, var, new_type));
            Type::Function(a, b)
        }
        Type::Tuple(ref types) => {
            let types = types.iter().map(|t| {
                substitute_var(t, var, new_type)
            }).collect();
            Type::Tuple(types)
        }
        Type::Var(v) if v == var => {
            new_type.clone()
        }
        Type::Var(var) => {
            Type::Var(var)
        }
    }
}

fn number_suffix(num: usize) -> &'static str {
    if num % 10 == 1 && num % 100 != 11 {
        "st"
    } else if num % 10 == 2 && num % 10 != 12 {
        "nd"
    } else if num % 10 == 3 && num % 10 != 13 {
        "rd"
    } else {
        "th"
    }
}

fn convert_resolved_type(type_: &r::Type, self_to: &Type) -> Type {
    match *type_ {
        r::Type::Any |
        r::Type::Concrete(Symbol::Unknown) => Type::Any,
        r::Type::Apply(ref a, ref b) => {
            let a = convert_resolved_type(&a.value, self_to);
            let b = convert_resolved_type(&b.value, self_to);
            Type::Apply(Rc::new(a), Rc::new(b))
        }
        r::Type::Concrete(Symbol::Known(sym)) => {
            Type::Concrete(sym)
        }
        r::Type::Function(ref a, ref b) => {
            let a = convert_resolved_type(&a.value, self_to);
            let b = convert_resolved_type(&b.value, self_to);
            Type::Function(Rc::new(a), Rc::new(b))
        }
        r::Type::SelfType => self_to.clone(),
        r::Type::Tuple(ref items) => {
            let items = items
                .iter()
                .map(|i| convert_resolved_type(&i.value, self_to))
                .collect();
            Type::Tuple(items)
        }
        r::Type::Var(sym) => Type::Var(sym.0),
    }
}

fn convert_resolved_scheme(scheme: &r::Scheme, self_to: &Type) -> Scheme {
    fn collect_vars<T: Default>(ty: &r::Type, to: &mut BTreeMap<u64, T>) {
        match *ty {
            r::Type::Any |
            r::Type::Concrete(_) |
            r::Type::SelfType => { }
            r::Type::Apply(ref a, ref b) |
            r::Type::Function(ref a, ref b) => {
                collect_vars(&a.value, to);
                collect_vars(&b.value, to);
            }
            r::Type::Tuple(ref items) => {
                for item in items {
                    collect_vars(&item.value, to);
                }
            }
            r::Type::Var(Sym(var)) => {
                to.entry(var).or_insert_with(Default::default);
            }
        }
    }
    let type_ = convert_resolved_type(&scheme.type_.value, self_to);
    let mut vars = BTreeMap::new();
    for &(ref var, ref trait_) in &scheme.bounds {
        if let Symbol::Known(sym) = trait_.value {
            vars.entry(var.value.0).or_insert_with(BTreeSet::new).insert(sym);
        }
    }
    collect_vars(&scheme.type_.value, &mut vars);
    let vars = vars
        .into_iter()
        .map(|(v, bounds)| SchemeVar {
            id: v,
            bounds: bounds
                .into_iter()
                .collect::<BTreeSet<_>>()
                .into_iter()
                .collect::<Vec<_>>(),
        })
        .collect();
    Scheme {
        vars,
        type_,
    }
}

fn convert_resolved_union(union: &r::UnionType) -> t::Union {
    let vars = union.vars.iter().map(|v| v.value.0).collect();
    let cases = union.cases
        .iter()
        .map(|case| {
            let tag = case.value.tag.clone();
            let types = case.value.args
                .iter()
                .map(|t| convert_resolved_type(&t.value, &Type::Any))
                .collect();
            (tag, types)
        })
        .collect();
    t::Union {
        name: union.name.clone(),
        vars,
        cases,
    }
}

fn convert_resolved_type_decl(decl: &r::TypeDecl) -> Option<t::TypeDecl> {
    match *decl {
        r::TypeDecl::Union(ref union) => {
            Some(t::TypeDecl::Union(convert_resolved_union(union)))
        }
        r::TypeDecl::TypeAlias(_) => {
            None
        }
    }
}

fn convert_resolved_trait(trait_: r::Trait) -> t::Trait {
    let r::Trait { name, base_traits, values } = trait_;
    let base_traits = base_traits
        .into_iter()
        .filter_map(|s| {
            if let Symbol::Known(sym) = s.value {
                Some(sym)
            } else {
                None
            }
        })
        .collect();
    let items = values
        .into_iter()
        .map(|t| {
            // TODO: `self` becoming `Type::Any` does not really make sense,
            // figure it out
            let scheme = convert_resolved_scheme(&t.value.type_.value, &Type::Any);
            (t.value.value.value, scheme)
        })
        .collect();
    t::Trait { name, base_traits, items }
}

fn collect_pattern_types(items: &r::GroupedItems) -> BTreeMap<Sym, PatternTy> {
    let mut types = BTreeMap::new();
    for type_ in &items.types {
        match *type_ {
            r::TypeDecl::Union(ref union) => {
                let vars = union.vars.iter().map(|v| v.value.0).collect::<Vec<_>>();
                let mut whole_type = Type::Concrete(union.name.value);
                for &var in &vars {
                    whole_type = Type::Apply(
                        Rc::new(whole_type),
                        Rc::new(Type::Var(var)));
                }
                for case in &union.cases {
                    let items = case.value.args
                        .iter()
                        .map(|c| convert_resolved_type(&c.value, &Type::Any))
                        .collect();
                    let pattern_ty = PatternTy {
                        vars: vars.clone(),
                        whole_type: whole_type.clone(),
                        items,
                    };
                    types.insert(case.value.tag.value, pattern_ty);
                }
            }
            r::TypeDecl::TypeAlias(_) => { }
        }
    }
    types
}

fn collect_constructor_types(items: &r::GroupedItems) -> BTreeMap<Sym, (Scheme, Span)> {
    let mut types = BTreeMap::new();
    for type_ in &items.types {
        match *type_ {
            r::TypeDecl::Union(ref union) => {
                let vars = union.vars.iter().map(|v| v.value.0).collect::<Vec<_>>();
                let mut result_type = Type::Concrete(union.name.value);
                let mut scheme_vars = Vec::new();
                for &var in &vars {
                    result_type = Type::Apply(
                        Rc::new(result_type),
                        Rc::new(Type::Var(var)));
                    scheme_vars.push(SchemeVar {
                        id: var,
                        bounds: Vec::new()
                    });
                }
                for case in &union.cases {
                    let mut constructor_type = result_type.clone();
                    for arg in case.value.args.iter().rev() {
                        let arg = convert_resolved_type(&arg.value, &Type::Any);
                        constructor_type = Type::Function(
                            Rc::new(arg),
                            Rc::new(constructor_type));
                    }
                    let scheme = Scheme {
                        vars: scheme_vars.clone(),
                        type_: constructor_type,
                    };
                    types.insert(case.value.tag.value, (scheme, case.value.tag.span));
                }
            }
            r::TypeDecl::TypeAlias(_) => { }
        }
    }
    types
}

fn collect_trait_item_types(items: &r::GroupedItems) -> BTreeMap<Sym, (Scheme, Span)> {
    let mut result = BTreeMap::new();
    for trait_ in &items.traits {
        for value in &trait_.values {
            let mut scheme = convert_resolved_scheme(&value.value.type_.value, &Type::Var(0));
            // add constraint for what used to be `self`
            scheme.vars.push(SchemeVar {
                id: 0,
                bounds: vec![trait_.name.value],
            });
            result.insert(value.value.value.value, (scheme, value.span));
        }
    }
    result
}

fn make_var_mapping(a: &Scheme, b: &Scheme) -> Option<BTreeMap<u64, u64>> {
    fn unify(a: &Type, b: &Type, mapping: &mut BTreeMap<u64, u64>) -> bool {
        match (a, b) {
            (&Type::Any, &Type::Any) => true,
            (&Type::Concrete(a), &Type::Concrete(b)) => a == b,
            (&Type::Apply(ref a1, ref a2), &Type::Apply(ref b1, ref b2)) |
            (&Type::Function(ref a1, ref a2), &Type::Function(ref b1, ref b2)) => {
                unify(a1, b1, mapping) && unify(a2, b2, mapping)
            }
            (&Type::Tuple(ref a), &Type::Tuple(ref b)) => {
                a.len() == b.len() && {
                    for (a, b) in a.iter().zip(b.iter()) {
                        if !unify(a, b, mapping) {
                            return false;
                        }
                    }
                    true
                }
            }
            (&Type::Var(a), &Type::Var(b)) => {
                *mapping.entry(a).or_insert(b) == b
            }
            _ => false,
        }
    }
    fn sorted<T: Clone + Ord>(v: &[T]) -> Vec<T> {
        let mut v = v.iter().cloned().collect::<Vec<_>>();
        v.sort();
        v
    }
    let mut var_mapping = BTreeMap::new();
    if !unify(&a.type_, &b.type_, &mut var_mapping) {
        return None;
    }
    if var_mapping.values().cloned().collect::<BTreeSet<_>>().len() != var_mapping.len() {
        return None;
    }
    let b_bounds = b.vars.iter().map(|v| (v.id, sorted(&v.bounds))).collect::<BTreeMap<_, _>>();
    for a_var in &a.vars {
        let b_var = var_mapping[&a_var.id];
        let expected_bounds = sorted(&a_var.bounds);
        if b_bounds.get(&b_var) != Some(&expected_bounds) {
            return None;
        }
    }
    Some(var_mapping)
}

fn are_schemes_equal(a: &Scheme, b: &Scheme) -> bool {
    make_var_mapping(a, b).is_some()
}

fn make_impl_annotations(
                            items: &r::GroupedItems,
                            trait_item_types: &BTreeMap<Sym, (Scheme, Span)>) -> BTreeMap<Sym, (Scheme, Span)> {
    let mut types = BTreeMap::new();
    for impl_ in &items.impls {
        let impl_scheme = convert_resolved_scheme(&impl_.scheme.value, &Type::Any);
        for def in impl_.values.iter().flat_map(|group| group.iter()) {
            let def_sym = def.sym.value;
            let trait_item_sym = if let Some(sym) = impl_.trait_items.get(&def_sym) {
                *sym
            } else {
                continue;
            };
            let &(ref scheme, span) = if let Some(s) = trait_item_types.get(&trait_item_sym) {
                s
            } else {
                continue;
            };
            let mut mapping = BTreeMap::new();
            mapping.insert(0, impl_scheme.type_.clone());
            let type_ = scheme.type_.map_vars(&mapping);
            let mut vars = scheme.vars.clone();
            vars.extend(impl_scheme.vars.iter().cloned());
            vars.retain(|v| v.id != 0);
            let scheme = Scheme {
                vars,
                type_,
            };
            types.insert(def_sym, (scheme, span));
        }
    }
    types
}

fn infer_impl(
                impl_: r::GroupedImpl,
                impl_item_types: &BTreeMap<Sym, (Scheme, Span)>,
                inferer: &mut InferCtx) -> t::Impl {
    let r::GroupedImpl { scheme, trait_, values, trait_items } = impl_;
    let mut typed_values = Vec::new();
    for group in &values {
        for def in inferer.infer_top_level_defs(group) {
            let mapping = match impl_item_types.get(&def.sym.value) {
                Some(&(ref scheme, _)) => {
                    make_var_mapping(scheme, &def.scheme)
                        .unwrap_or_default()
                }
                None => BTreeMap::new(),
            };
            typed_values.push(ImplDef {
                def,
                var_mapping: mapping,
            });
        }
    }
    let scheme = Node::new(convert_resolved_scheme(&scheme.value, &Type::Any), scheme.span);
    t::Impl {
        symbol: ImplSym(0),
        scheme,
        trait_,
        items: typed_values,
        trait_items,
    }
}

pub(crate) fn infer_types(mut items: r::GroupedItems, ctx: &mut CompileCtx) -> t::Items {
    let pattern_types = collect_pattern_types(&items);
    let trait_item_types = collect_trait_item_types(&items);
    let impl_item_types = make_impl_annotations(&items, &trait_item_types);
    let mut known_types = collect_constructor_types(&items);
    known_types.extend(impl_item_types.clone());
    known_types.extend(trait_item_types);
    known_types.extend(items.annotations
        .iter()
        .map(|(&sym, annot)| {
            let scheme = convert_resolved_scheme(&annot.value.type_.value, &Type::Any);
            (sym, (scheme, annot.span))
        }));
    
    let (defs, impls, mut symbol_types) = {
        let mut inferer = InferCtx::new(
            &pattern_types,
            &known_types,
            ctx);
        
        let mut typed_defs = Vec::new();
        for def_group in &items.items {
            let typed = inferer.infer_top_level_defs(def_group);
            typed_defs.extend(typed);
        }
        
        let mut typed_impls = Vec::new();
        let mut next_meta_sym = 0;
        for impl_ in items.impls.drain(..) {
            let mut impl_ = infer_impl(impl_, &impl_item_types, &mut inferer);
            impl_.symbol = ImplSym(next_meta_sym);
            next_meta_sym += 1;
            typed_impls.push(impl_);
        }

        (typed_defs, typed_impls, inferer.env)
    };

    for (sym, (typ, _)) in known_types {
        symbol_types.entry(sym).or_insert(typ);
    }

    let traits = items.traits
        .drain(..)
        .map(convert_resolved_trait)
        .collect();

    let types = items.types
        .iter()
        .filter_map(convert_resolved_type_decl)
        .collect();

    for def in &defs {
        if let Some(annot) = items.annotations.get(&def.sym.value) {
            let scheme = convert_resolved_scheme(&annot.value.type_.value, &Type::Any);
            if !are_schemes_equal(&def.scheme, &scheme) {
                let msg1 = format!(
                    "Infered type of `{}` is less general than annotated. Infered type `{}`,",
                    ctx.symbols.symbol_name(annot.value.value.value),
                    def.scheme.display(&ctx.symbols));
                let msg2 = format!(
                    "while annotation says it should be `{}`.",
                    scheme.display(&ctx.symbols));
                ctx.reporter
                    .type_error(msg1.as_str(), def.sym.span)
                    .span_note(msg1, def.sym.span)
                    .span_note(msg2, annot.value.value.span)
                    .done();
            }
        }
    }

    for def in impls.iter().flat_map(|i| i.items.iter()) {
        if let Some(&(ref scheme, span)) = impl_item_types.get(&def.def.sym.value) {
            if !are_schemes_equal(&def.def.scheme, scheme) {
                let msg1 = format!(
                    "Infered type of `{}` is less general than required. Infered type `{}`,",
                    ctx.symbols.symbol_name(def.def.sym.value),
                    def.def.scheme.display(&ctx.symbols));
                let msg2 = format!(
                    "while annotation says it should be `{}`.",
                    scheme.display(&ctx.symbols));
                ctx.reporter
                    .type_error(msg1.as_str(), def.def.sym.span)
                    .span_note(msg1, def.def.sym.span)
                    .span_note(msg2, span)
                    .done();
            }
        }
    }
    
    t::Items {
        types: types,
        traits,
        impls,
        items: defs,
        symbol_types: symbol_types,
    }
}
