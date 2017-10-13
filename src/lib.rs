extern crate codemap;

mod ast;
mod symbols;
mod parsing;
mod compiler;
mod reporting;
pub mod diagnostics;
pub mod vm;

use codemap::CodeMap;
pub use parsing::{SourceProvider, HashMapProvider};
use diagnostics::{Diagnostics};
use reporting::Reporter;
use symbols::SymbolSource;
use vm::Vm;


pub struct CompileResult {
    pub vm: Option<Vm>,
    pub diagnostics: Diagnostics,
}

pub struct CompileParams<'a, S: 'a> {
    main: &'a str,
    provider: &'a mut S,
}

impl<'a, S: SourceProvider + 'a> CompileParams<'a, S> {
    pub fn new(main: &'a str, provider: &'a mut S) -> Self {
        CompileParams {
            main,
            provider,
        }
    }
}

pub fn compile<'a, S>(params: CompileParams<'a, S>) -> CompileResult
    where S: SourceProvider + 'a
{
    let mut ctx = CompileCtx::new();

    let modules = parsing::parse_modules(params.main, params.provider, &mut ctx);
    let vm = compiler::compile(&modules, &mut ctx).ok();

    let diagnostics = Diagnostics::new(
        ctx.reporter.into_diagnostics(),
        ctx.codemap,
    );

    CompileResult {
        vm,
        diagnostics,
    }
}

pub fn check<'a, S>(params: CompileParams<'a, S>) -> Diagnostics
    where S: SourceProvider + 'a
{
    compile(params).diagnostics
}

struct CompileCtx {
    reporter: Reporter,
    symbols: SymbolSource,
    codemap: CodeMap,
}

impl CompileCtx {
    fn new() -> CompileCtx {
        CompileCtx {
            reporter: Reporter::new(),
            symbols: SymbolSource::new(),
            codemap: CodeMap::new(),
        }
    }
}
