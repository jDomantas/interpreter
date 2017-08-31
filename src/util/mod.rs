pub mod position;
pub mod errors;
pub mod symbols;

use self::errors::Errors;
use self::symbols::SymbolSource;


pub struct CompileCtx {
    pub errors: Errors,
    pub symbols: SymbolSource,
}

impl CompileCtx {
    pub fn new() -> CompileCtx {
        CompileCtx {
            errors: Errors::new(),
            symbols: SymbolSource::new(),
        }
    }

    pub fn into_errors(self) -> Errors {
        self.errors
    }
}
