use position::Span;


pub struct Error {
    pub module: String,
    pub message: String,
    pub span: Span,
    pub phase: Phase,
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Copy, Clone, Hash)]
pub enum Phase {
    Parsing,
    SymbolResolution,
    TypeChecking,
}

pub fn parse_error<T, U>(message: T, span: Span, module: U) -> Error
        where T: Into<String>, U: Into<String> {
    Error {
        module: module.into(),
        message: message.into(),
        span: span,
        phase: Phase::Parsing,
    }
}

pub fn symbol_error<T, U>(message: T, span: Span, module: U) -> Error
        where T: Into<String>, U: Into<String> {
    unimplemented!()
}

pub fn double_symbol_error<T, U, V>(message: T, span: Span, previous_msg: U, previous: Span, module: V) -> Error
        where T: Into<String>, U: Into<String>, V: Into<String> {
    unimplemented!()
}
