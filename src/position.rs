use std::cmp;


#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn new(line: usize, column: usize) -> Position {
        Position {
            line: line,
            column: column,
        }
    }

    pub fn span_to(&self, end: &Position) -> Span {
        Span::new(self, end)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: &Position, end: &Position) -> Span {
        Span {
            start: start.clone(),
            end: end.clone(),
        }
    }

    pub fn merge(&self, other: &Span) -> Span {
        let start = cmp::min(&self.start, &other.start);
        let end = cmp::max(&self.end, &other.end);
        Span::new(start, end)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(value: T, span: Span) -> Spanned<T> {
        Spanned {
            value: value,
            span: span,
        }
    }
}

pub static DUMMY_SPAN: Span = Span {
    start: Position { line: 0, column: 0 },
    end: Position { line: 0, column: 0 },
};
