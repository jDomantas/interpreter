use std::cmp::Ordering;
use ast::Name;
use position::Span;


#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Error {
    pub module: Name,
    pub notes: Vec<Note>,
    pub phase: Phase,
}

impl Error {
    pub fn main_span(&self) -> Span {
        self.notes[0].span
    }

    pub fn ordering(&self, other: &Error) -> Ordering {
        let by_phase = self.phase.cmp(&other.phase);
        let by_module = self.module.cmp(&other.module);
        let by_position = self.main_span().start.cmp(&other.main_span().start);
        by_phase.then(by_module).then(by_position)
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Copy, Clone, Hash)]
pub enum Phase {
    Parsing,
    SymbolResolution,
    FixityResolution,
    KindChecking,
    TypeAliasExpansion,
    TypeChecking,
}

#[derive(Default, Debug)]
pub struct Errors {
    errors: Vec<Error>,
}

impl Errors {
    pub fn new() -> Errors {
        Default::default()
    }

    pub fn new_error(&mut self, phase: Phase, module: &Name) -> ErrorBuilder {
        ErrorBuilder::new(self, phase, module.clone())
    }

    pub fn parse_error(&mut self, module: &Name) -> ErrorBuilder {
        self.new_error(Phase::Parsing, module)
    }

    pub fn symbol_error(&mut self, module: &Name) -> ErrorBuilder {
        self.new_error(Phase::SymbolResolution, module)
    }

    pub fn alias_expansion_error(&mut self, module: &Name) -> ErrorBuilder {
        self.new_error(Phase::TypeAliasExpansion, module)
    }

    pub fn fixity_error(&mut self, module: &Name) -> ErrorBuilder {
        self.new_error(Phase::FixityResolution, module)
    }

    pub fn kind_error(&mut self, module: &Name) -> ErrorBuilder {
        self.new_error(Phase::KindChecking, module)
    }

    pub fn into_error_list(mut self) -> Vec<Error> {
        self.errors.sort_by(Error::ordering);
        self.errors
    }

    pub fn have_errors(&self) -> bool {
        self.error_count() > 0
    }

    pub fn error_count(&self) -> usize {
        self.errors.len()
    }

    pub fn merge(&mut self, other: Errors) {
        self.errors.extend(other.errors)
    }
}

#[must_use]
pub struct ErrorBuilder<'a> {
    errors: &'a mut Errors,
    error: Error,
}

impl<'a> ErrorBuilder<'a> {
    fn new(errors: &'a mut Errors, phase: Phase, module: Name) -> Self {
        ErrorBuilder {
            errors: errors,
            error: Error {
                phase: phase,
                module: module,
                notes: Vec::new(),
            },
        }
    }

    pub fn note<S: Into<String>>(mut self, msg: S, span: Span) -> Self {
        self.error.notes.push(Note::new(msg, span));
        self
    }

    pub fn done(mut self) {
        if self.error.notes.len() == 0 {
            panic!("Built an error message without any notes.");
        }
        self.errors.errors.push(self.error);
    }
}
