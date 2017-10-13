mod emitter;

use std::io::{self, Write};
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
    raw_diagnostics: Vec<Diagnostic<::codemap::Span>>,
    codemap: CodeMap,
}

impl Diagnostics {
    pub(crate) fn new(
        mut raw_diagnostics: Vec<Diagnostic<::codemap::Span>>,
        codemap: CodeMap,
    ) -> Self {
        raw_diagnostics.sort_by_key(|d| {
            d.primary_span.map(|span| {
                let file = codemap.find_file(span.low()).name();
                let loc = codemap.look_up_pos(span.low()).position;
                (d.phase, file, loc.line, loc.column)
            })
        });
        Diagnostics {
            raw_diagnostics,
            codemap,
        }
    }

    pub fn into_vec(self) -> Vec<Diagnostic<Span>> {
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

    pub fn emit<W: Write>(&self, to: W) -> io::Result<()> {
        emitter::print_diagnostics(&self.codemap, &self.raw_diagnostics, to)
    }
}
