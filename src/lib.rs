// TODO: don't forget to remove after developing
#![allow(dead_code)]

pub mod util;
pub(crate) mod parsing;
pub(crate) mod compiler;
pub(crate) mod ast;
pub mod vm;

pub use parsing::{SourceProvider, HashMapProvider};
use util::CompileCtx;
use util::errors::Errors;
use vm::Vm;


pub fn compile<S>(provider: &S, main: &str) -> Result<Vm, Errors>
    where S: SourceProvider
{
    let mut ctx = CompileCtx::new();

    let modules = parsing::parse_modules(main, provider, &mut ctx);
    if ctx.errors.have_errors() {
        return Err(ctx.into_errors());
    }

    compiler::compile(&modules, &mut ctx).map_err(|()| ctx.into_errors())
}
