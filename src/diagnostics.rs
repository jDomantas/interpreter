use codemap::CodeMap;


#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Copy, Hash)]
pub struct Pos {
    pub line: usize,
    pub col: usize,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub struct Span {
    pub module: String,
    pub start: Pos,
    pub end: Pos,
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum Severity {
    Error,
    Warning,
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Copy, Clone, Hash)]
pub enum Phase {
    Parsing,
    SymbolResolution,
    FixityResolution,
    KindChecking,
    TypeAliasExpansion,
    PatternError,
    TypeChecking,
    TraitChecking,
}

#[derive(Debug, Clone)]
pub struct Diagnostic<Loc> {
    pub message: String,
    pub primary_span: Option<Loc>,
    pub severity: Severity,
    pub phase: Phase,
    pub notes: Vec<Note<Loc>>,
}

impl<Loc> Diagnostic<Loc> {
    pub(crate) fn translate_loc<T, F: Fn(Loc) -> T>(self, f: F) -> Diagnostic<T> {
        let notes = self.notes.into_iter().map(|n| n.translate_loc(&f)).collect();
        let primary_span = self.primary_span.map(|loc| f(loc));
        Diagnostic {
            message: self.message,
            primary_span,
            severity: self.severity,
            phase: self.phase,
            notes,
        }
    }
}

impl<Loc: Ord> Diagnostic<Loc> {
    pub fn ordering(&self, other: &Self) -> ::std::cmp::Ordering {
        let by_phase = self.phase.cmp(&other.phase);
        let by_position = self.primary_span.cmp(&other.primary_span);
        by_phase.then(by_position)
    }
}

#[derive(Debug, Clone)]
pub struct Note<Loc> {
    pub span: Loc,
    pub message: Option<String>,
}

impl<Loc> Note<Loc> {
    pub(crate) fn translate_loc<T, F: Fn(Loc) -> T>(self, f: F) -> Note<T> {
        let span = f(self.span);
        Note {
            span,
            message: self.message,
        }
    }
}

pub struct Diagnostics {
    pub(crate) raw_diagnostics: Vec<Diagnostic<::codemap::Span>>,
    pub(crate) codemap: CodeMap,
}

impl Diagnostics {
    pub fn into_diagnostics(self) -> Vec<Diagnostic<Span>> {
        let Diagnostics { raw_diagnostics, codemap } = self;
        let translator = |span| {
            let spanloc = codemap.look_up_span(span);
            Span {
                module: spanloc.file.name().into(),
                start: Pos {
                    line: spanloc.begin.line,
                    col: spanloc.begin.column,
                },
                end: Pos {
                    line: spanloc.end.line,
                    col: spanloc.end.column,
                },
            }
        };
        raw_diagnostics
            .into_iter()
            .map(|d| d.translate_loc(&translator))
            .collect()
    }
}
