// TODO: don't forget to remove after developing
#![allow(dead_code)]

pub mod util;
pub mod parsing;
pub mod compiler;
pub mod ast;
pub mod vm;

use std::collections::BTreeMap;
use parsing::SourceProvider;
use util::CompileCtx;
use util::errors::Errors;
use util::symbols::Sym;
use vm::{Function, GlobalValue};


pub fn compile<S: SourceProvider>(provider: &S, main: &str)
    -> Result<(BTreeMap<u64, Function>, BTreeMap<Sym, GlobalValue>), Errors>
{
    let mut ctx = CompileCtx::new();

    let modules = parsing::parse_modules(main, provider, &mut ctx);
    if ctx.errors.have_errors() {
        return Err(ctx.into_errors());
    }

    compiler::compile(&modules, &mut ctx).map_err(|()| ctx.into_errors())
}
