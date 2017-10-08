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

#[derive(Debug, Clone)]
pub struct Note<Loc> {
    pub span: Loc,
    pub message: Option<String>,
}


