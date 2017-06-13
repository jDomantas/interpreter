use position::Span;


pub struct ParseError {
    pub message: String,
    pub span: Span,
}

pub struct ModuleLoadError {
    pub module: String,
    pub span: Span,
}

