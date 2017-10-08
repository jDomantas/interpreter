mod ast;
mod symbols;
mod parsing;
mod compiler;
mod reporting;
pub mod position;
pub mod diagnostics;
pub mod vm;

pub use parsing::{SourceProvider, HashMapProvider};
use diagnostics::Diagnostic;
use position::Span;
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

    CompileResult {
        vm,
        diagnostics: ctx.reporter.into_diagnostics(),
    }
}

struct CompileCtx {
    reporter: Reporter,
    symbols: SymbolSource,
}

impl CompileCtx {
    fn new() -> CompileCtx {
        CompileCtx {
            reporter: Reporter::new(),
            symbols: SymbolSource::new(),
        }
    }
}
