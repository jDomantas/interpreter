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
use diagnostics::{Diagnostic, Diagnostics, Span};
use reporting::Reporter;
use symbols::SymbolSource;
use vm::Vm;


pub struct CompileResult {
    pub vm: Option<Vm>,
    pub diagnostics: Vec<Diagnostic<Span>>,
}

pub fn compile<S>(provider: &S, main: &str) -> CompileResult
    where S: SourceProvider
{
    let mut ctx = CompileCtx::new();

    let modules = parsing::parse_modules(main, provider, &mut ctx);
    let vm = compiler::compile(&modules, &mut ctx).ok();

    let diagnostics = Diagnostics {
        raw_diagnostics: ctx.reporter.into_diagnostics(),
        codemap: ctx.codemap,
    };

    CompileResult {
        vm,
        diagnostics: diagnostics.into_diagnostics(),
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
