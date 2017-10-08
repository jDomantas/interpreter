use position::Span;
use diagnostics::{Severity, Phase, Diagnostic, Note};



#[derive(Default, Debug)]
pub struct Reporter {
    diagnostics: Vec<Diagnostic<Span>>,
}

macro_rules! error_builder {
    ($name:ident, $phase:expr) => {
        pub fn $name<S: Into<String>>(
            &mut self,
            message: S,
            primary_span: Span,
        ) -> Builder {
            self.new_error($phase, message, primary_span)
        }
    }
}

impl Reporter {
    pub fn new() -> Reporter {
        Default::default()
    }

    pub fn new_error<S: Into<String>>(
        &mut self,
        phase: Phase,
        message: S,
        primary_span: Span,
    ) -> Builder {
        Builder::new(self, Severity::Error, phase, message.into(), primary_span)
    }

    error_builder!(parse_error, Phase::Parsing);
    error_builder!(symbol_error, Phase::SymbolResolution);
    error_builder!(alias_expansion_error, Phase::TypeAliasExpansion);
    error_builder!(fixity_error, Phase::FixityResolution);
    error_builder!(kind_error, Phase::KindChecking);
    error_builder!(pattern_error, Phase::PatternError);
    error_builder!(type_error, Phase::TypeChecking);
    error_builder!(trait_error, Phase::TraitChecking);

    pub fn into_diagnostics(self) -> Vec<Diagnostic<Span>> {
        self.diagnostics
    }

    pub fn have_errors(&self) -> bool {
        self.error_count() > 0
    }

    pub fn error_count(&self) -> usize {
        self.diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .count()
    }
}

#[must_use]
pub struct Builder<'a> {
    reporter: &'a mut Reporter,
    diagnostic: Diagnostic<Span>,
}

impl<'a> Builder<'a> {
    fn new(
        reporter: &'a mut Reporter,
        severity: Severity,
        phase: Phase,
        msg: String,
        primary_span: Span
    ) -> Self {
        Builder {
            reporter,
            diagnostic: Diagnostic {
                message: msg,
                severity,
                phase,
                primary_span: Some(primary_span),
                notes: Vec::new(),
            },
        }
    }

    pub fn done(self) {
        assert!(!self.diagnostic.notes.is_empty(), "built a diagnostic without any notes");
        self.reporter.diagnostics.push(self.diagnostic);
    }

    #[allow(dead_code)]
    pub fn span(self, span: Span) -> Self {
        self.note(None, span)
    }

    #[allow(dead_code)]
    pub fn span_note<T>(self, msg: T, span: Span) -> Self
        where T: Into<String>
    {
        self.note(Some(msg.into()), span)
    }

    fn note(mut self, msg: Option<String>, span: Span) -> Self {
        self.diagnostic.notes.push(Note {
            span,
            message: msg,
        });
        self
    }
}
