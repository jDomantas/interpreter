use std::cmp::Ordering;
use position::Span;


pub struct Note {
    pub message: String,
    pub span: Span,
}

impl Note {
    fn new<S: Into<String>>(message: S, span: Span) -> Note {
        Note {
            message: message.into(),
            span: span,
        }
    }
}

pub struct Error {
    pub module: String,
    pub notes: Vec<Note>,
    pub phase: Phase,
}

impl Error {
    pub fn span(&self) -> Span {
        self.notes[0].span
    }

    pub fn ordering(&self, other: &Error) -> Ordering {
        let by_module = self.module.cmp(&other.module);
        let by_position = self.span().start.cmp(&other.span().start);
        by_module.then(by_position)
    }
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
        notes: vec![Note::new(message, span)],
        phase: Phase::Parsing,
    }
}

pub fn symbol_error<T, U>(message: T, span: Span, module: U) -> Error
        where T: Into<String>, U: Into<String> {
    Error {
        module: module.into(),
        notes: vec![Note::new(message, span)],
        phase: Phase::SymbolResolution,
    }
}

pub fn double_symbol_error<T, U, V>(message: T, span: Span, previous_msg: U, previous: Span, module: V) -> Error
        where T: Into<String>, U: Into<String>, V: Into<String> {
    Error {
        module: module.into(),
        notes: vec![Note::new(message, span), Note::new(previous_msg, previous)],
        phase: Phase::SymbolResolution,
    }
}

pub fn module_not_loaded<T, U>(message: T, span: Span, module: U) -> Error
        where T: Into<String>, U: Into<String> {
    Error {
        module: module.into(),
        notes: vec![Note::new(message, span)],
        phase: Phase::Parsing,
    }
}
