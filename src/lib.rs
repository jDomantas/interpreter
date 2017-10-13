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

pub fn compile<S>(provider: &S, main: &str) -> CompileResult
    where S: SourceProvider
{
    let mut ctx = CompileCtx::new();

    let modules = parsing::parse_modules(main, provider, &mut ctx);
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
