use std::collections::HashMap;
use ast::typed as t;
use ast::resolved as r;
use ast::Node;
use errors::Error;
use position::Span;


enum ConstraintSource {
    IfCondition(Span),
    IfBranches(Span, Span),
    MatchBranches(u64, Span, Span),
}

struct Constraint(Type, Type, ConstraintSource);

struct Scheme(Vec<u64>, Type);

struct InferCtx<'a> {
    env: HashMap<&str, Scheme>,
    next_var: u64,
    errors: Vec<Error>,
    constraints: Vec<Constraint>,
}

impl InferCtx<'a> {
    fn new() -> Self {
        InferCtx {
            env: HashMap::new(),
            next_var: 0,
            errors: Vec::new(),
        }
    }

    fn infer_defs(defs: &[r::Def]) {
        
    }
}
