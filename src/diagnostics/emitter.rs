use std::collections::{BTreeMap, HashMap, HashSet};
use std::io::{self, Write};
use std::sync::Arc;
use codemap::{CodeMap, File, Span};
use super::{Diagnostic, Note, Severity};


#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Copy, Clone)]
enum MarkerStyle {
    // ~~~~~ or ____~
    Secondary,
    // ^^^^^ or ____^
    Primary,
}

impl MarkerStyle {
    fn from_note_index(index: usize) -> MarkerStyle {
        if index == 0 {
            MarkerStyle::Primary
        } else {
            MarkerStyle::Secondary
        }
    }

    fn to_marker_part(self, is_arrow: bool) -> MarkerPart {
        match self {
            MarkerStyle::Primary => {
                if is_arrow {
                    MarkerPart::ArrowPrimary
                } else {
                    MarkerPart::PrimaryMarker
                }
            }
            MarkerStyle::Secondary => {
                if is_arrow {
                    MarkerPart::ArrowSecondary
                } else {
                    MarkerPart::SecondaryMarker
                }
            }
        }
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Copy, Clone)]
enum MarkerPart {
    None,
    ArrowBottom,
    SecondaryMarker,
    PrimaryMarker,
    ArrowSecondary,
    ArrowPrimary,
    SingleColumn
}

impl MarkerPart {
    fn or(self, other: MarkerPart) -> MarkerPart {
        ::std::cmp::max(self, other)
    }

    fn to_char(self) -> char {
        match self {
            MarkerPart::None => ' ',
            MarkerPart::ArrowBottom => '_',
            MarkerPart::SecondaryMarker |
            MarkerPart::ArrowSecondary => '~',
            MarkerPart::PrimaryMarker |
            MarkerPart::ArrowPrimary => '^',
            MarkerPart::SingleColumn => '|',
        }
    }
}

#[derive(Debug, Clone)]
enum LineMarker<'a> {
    FromStart {
        connect_col: usize,
        arrow_col: usize,
        message: Option<&'a str>,
        style: MarkerStyle,
    },
    FromTo {
        start_col: usize,
        end_col: usize,
        message: Option<&'a str>,
        style: MarkerStyle,
    },
}

impl<'a> LineMarker<'a> {
    fn message(&self) -> Option<&str> {
        match *self {
            LineMarker::FromStart { message, .. } |
            LineMarker::FromTo { message, .. } => message,
        }
    }

    fn end_col(&self) -> usize {
        match *self {
            LineMarker::FromStart { arrow_col, .. } => arrow_col + 1,
            LineMarker::FromTo { end_col, .. } => end_col,
        }
    }

    fn is_arrow(&self) -> bool {
        match *self {
            LineMarker::FromStart { .. } => true,
            LineMarker::FromTo { .. } => false,
        }
    }
}

struct Printer<'a> {
    file: Arc<File>,
    // use BTreeMap here so that we can iterate annotated lines in order
    line_markers: BTreeMap<usize, Vec<LineMarker<'a>>>,
    next_connect_col: usize,
    full_connection_cols: HashSet<usize>,
    number_width: usize,
}

impl<'a> Printer<'a> {
    fn new(file: Arc<File>, number_width: usize) -> Self {
        Printer {
            file,
            line_markers: BTreeMap::new(),
            next_connect_col: 0,
            full_connection_cols: HashSet::new(),
            number_width,
        }
    }

    fn print_notes<W: Write>(
        &mut self,
        codemap: &CodeMap,
        notes: &'a [Note<Span>],
        mut output: W,
    ) -> io::Result<()> {
        assert!(self.line_markers.is_empty());
        assert_eq!(self.next_connect_col, 0);
        assert!(self.full_connection_cols.is_empty());
        for (index, note) in notes.iter().enumerate() {
            self.add_note_markers(codemap, note, MarkerStyle::from_note_index(index));
        }
        for markers in self.line_markers.values_mut() {
            markers.sort_by_key(LineMarker::end_col);
        }
        assert!(!self.line_markers.is_empty());
        writeln!(
            output,
            "{: <width$}--> module: {}",
            ' ',
            self.file.name(),
            width = self.number_width,
        )?;
        self.print_annotations(output)?;
        self.line_markers.clear();
        assert!(self.full_connection_cols.is_empty());
        self.next_connect_col = 0;
        Ok(())
    }

    fn add_note_markers(&mut self, codemap: &CodeMap, note: &'a Note<Span>, style: MarkerStyle) {
        let span = codemap.look_up_span(note.span);
        if span.begin.line == span.end.line {
            let start_col = span.begin.column + 1;
            let end_col = if start_col == span.end.column + 1 {
                start_col + 1
            } else {
                span.end.column + 1
            };
            self.add_line_marker(span.begin.line + 1, LineMarker::FromTo {
                start_col,
                end_col,
                message: note.message.as_ref().map(AsRef::as_ref),
                style,
            });
        } else {
            let connect_col = self.next_connect_col;
            self.next_connect_col += 2;
            self.add_line_marker(span.begin.line + 1, LineMarker::FromStart {
                connect_col,
                arrow_col: span.begin.column + 1,
                message: None,
                style,
            });
            self.add_line_marker(span.end.line + 1, LineMarker::FromStart {
                connect_col,
                arrow_col: span.end.column,
                message: note.message.as_ref().map(AsRef::as_ref),
                style,
            });
        }
    }

    fn add_line_marker(&mut self, line: usize, marker: LineMarker<'a>) {
        self.line_markers
            .entry(line)
            .or_insert_with(Vec::new)
            .push(marker);
    }

    fn print_annotations<W: Write>(&mut self, mut output: W) -> io::Result<()> {
        let line_markers = ::std::mem::replace(&mut self.line_markers, BTreeMap::new());
        let mut last_printed = None;
        for (line, markers) in line_markers {
            if let Some(prev) = last_printed {
                if !self.full_connection_cols.is_empty() {
                    if line - prev > 3 {
                        self.print_line(prev + 1, &mut output)?;
                        self.print_gap_line(&mut output)?;
                        self.print_line(line - 1, &mut output)?;
                    } else {
                        for line in (prev + 1)..line {
                            self.print_line(line, &mut output)?;
                        }
                    }
                } else if line - prev > 1 {
                    self.print_gap_line(&mut output)?;
                }
            }
            last_printed = Some(line);
            self.print_line_and_markers(line, markers, &mut output)?;
        }
        Ok(())
    }

    fn print_line_and_markers<W: Write>(
        &mut self,
        line: usize,
        mut markers: Vec<LineMarker>,
        mut output: W,
    ) -> io::Result<()> {
        assert!(!markers.is_empty(), "line has no markers");
        self.print_line(line, &mut output)?;
        self.print_immediate_markers(&markers, &mut output)?;
        // last span has its message printed inline
        writeln!(output, " {}", markers
            .iter()
            .next_back()
            .unwrap()
            .message()
            .unwrap_or(""))?;
        markers.pop();
        
        // non-arrow markers without messages don't extend below first line
        markers.retain(|m| m.message().is_some() || m.is_arrow());
        
        // possibly print additional line so that instead of
        //      let x = 1;
        //          ~~~  ~ first note
        //            second note
        // we would get
        //      let x = 1;
        //          ~~~  ~ first note
        //            |
        //            second note
        match markers.iter().next_back().cloned() {
            Some(LineMarker::FromTo { end_col, .. }) => {
                self.print_markers(&markers, end_col, &mut output)?;
                writeln!(output)?;
            }
            Some(LineMarker::FromStart { arrow_col, message, .. })
                if message.is_some() =>
            {
                self.print_markers(&markers, arrow_col + 1, &mut output)?;
                writeln!(output)?;
            }
            _ => {}
        }
        for i in (1..(markers.len() + 1)).rev() {
            let markers = &markers[0..i];
            match markers[i - 1] {
                LineMarker::FromTo { end_col, message, .. } => {
                    self.print_markers(markers, end_col - 1, &mut output)?;
                    writeln!(output, "{}", message.unwrap_or(""))?;
                }
                LineMarker::FromStart { arrow_col, message, .. } => {
                    self.print_markers(markers, arrow_col + 2, &mut output)?;
                    writeln!(output, "{}", message.unwrap_or(""))?;
                }
            }
        }
        Ok(())
    }

    fn print_line<W: Write>(&mut self, line: usize, mut output: W) -> io::Result<()> {
        self.print_line_header(Some(line), None, &mut output)?;
        for ch in self.file.source_line(line - 1).chars() {
            match ch {
                // print these chars as one space, otherwise
                // they will break note alignment with code
                '\t' | '\r' => write!(output, " ")?,
                ch => write!(output, "{}", ch)?,
            }
        }
        writeln!(output)
    }

    fn print_line_header<W: Write>(
        &mut self,
        line: Option<usize>,
        arrow_from: Option<usize>,
        mut output: W,
    ) -> io::Result<()> {
        if let Some(line) = line {
            write!(output, "{: >width$} |  ", line, width = self.number_width)?;
        } else {
            write!(output, "{: >width$} |  ", "", width = self.number_width)?;
        }
        for col in 0..self.next_connect_col {
            if self.full_connection_cols.contains(&col) {
                write!(output, "|")?;
            } else {
                match arrow_from {
                    Some(c) if c < col => write!(output, "_")?,
                    _ => write!(output, " ")?,
                }
            }
            if arrow_from == Some(col) {
                if self.full_connection_cols.contains(&col) {
                    self.full_connection_cols.remove(&col);
                } else {
                    self.full_connection_cols.insert(col);
                }
            }
        }
        Ok(())
    }

    fn print_gap_line<W: Write>(&mut self, mut output: W) -> io::Result<()> {
        write!(output, "{: <width$}", "...", width = self.number_width + 4)?;
        for col in 0..self.next_connect_col {
            if self.full_connection_cols.contains(&col) {
                write!(output, "|")?;
            } else {
                write!(output, " ")?;
            }
        }
        writeln!(output)
    }

    fn print_immediate_markers<W: Write>(
        &mut self,
        markers: &[LineMarker],
        mut output: W,
    ) -> io::Result<()> {
        let (connect, arrow_end) = match *markers.iter().next_back().unwrap() {
            LineMarker::FromStart { connect_col, arrow_col, .. } => {
                (Some(connect_col), arrow_col.saturating_sub(1))
            }
            _ => {
                (None, 0)
            }
        };
        self.print_line_header(None, connect, &mut output)?;
        let last_col = markers.iter().next_back().unwrap().end_col();
        for col in 1..last_col {
            let mut part = if col <= arrow_end {
                MarkerPart::ArrowBottom
            } else {
                MarkerPart::None
            };
            for (index, marker) in markers.iter().rev().enumerate() {
                match *marker {
                    LineMarker::FromStart { arrow_col, style, .. } => {
                        if arrow_col == col {
                            part = part.or(style.to_marker_part(true));
                        }
                    }
                    LineMarker::FromTo { start_col, end_col, style, message } => {
                        if start_col <= col && col < end_col {
                            part = part.or(style.to_marker_part(false));
                        }
                        if start_col == col
                            && end_col == start_col + 1 // marker is one col wide
                            && index > 0 // and not the last one in line
                            && message.is_some() // and has a message
                            && style != MarkerStyle::Primary // and it is not primary
                        {
                            part = part.or(MarkerPart::SingleColumn);
                        }
                    }
                }
            }
            write!(output, "{}", part.to_char())?;
        }
        Ok(())
    }

    fn print_markers<W: Write>(
        &mut self,
        markers: &[LineMarker],
        last_col: usize,
        mut output: W,
    ) -> io::Result<()> {
        let (connect, arrow_end) = match *markers.iter().next_back().unwrap() {
            LineMarker::FromStart { connect_col, arrow_col, .. } => {
                (Some(connect_col), arrow_col - 1)
            }
            _ => {
                (None, 0)
            }
        };
        self.print_line_header(None, connect, &mut output)?;
        for col in 1..last_col {
            let mut part = if col <= arrow_end {
                MarkerPart::ArrowBottom
            } else {
                MarkerPart::None
            };
            for marker in markers.iter().rev() {
                match *marker {
                    LineMarker::FromStart { arrow_col, .. } => {
                        if arrow_col == col {
                            part = MarkerPart::SingleColumn;
                        }
                    }
                    LineMarker::FromTo { end_col, message, .. } => {
                        if end_col == col + 1 && message.is_some() {
                            part = MarkerPart::SingleColumn;
                        }
                    }
                }
            }
            write!(output, "{}", part.to_char())?;
        }
        Ok(())
    }
}

struct MultiFilePrinter<'a, W> {
    codemap: &'a CodeMap,
    printers: HashMap<&'a str, Printer<'a>>,
    output: W,
    number_width: usize,
}

impl<'a, W: Write> MultiFilePrinter<'a, W> {
    fn new(codemap: &'a CodeMap, output: W) -> Self {
        MultiFilePrinter {
            codemap,
            printers: HashMap::new(),
            output,
            number_width: 0,
        }
    }

    fn emit_diagnostics(mut self, diagnostics: &'a [Diagnostic<Span>]) -> io::Result<()> {
        self.number_width = number_length(diagnostics
            .iter()
            .map(|d| max_line_number(self.codemap, d))
            .fold(0, ::std::cmp::max));
        
        for diagnostic in diagnostics {
            let severity = match diagnostic.severity {
                Severity::Error => "error",
                Severity::Warning => "warning",
            };
            writeln!(self.output, "{}: {}", severity, diagnostic.message)?;
            self.emit_notes(&diagnostic.notes)?;
            writeln!(self.output)?;
        }
        Ok(())
    }

    fn emit_notes(&mut self, notes: &'a [Note<Span>]) -> io::Result<()> {
        let mut prev_start = 0;
        for index in 0..notes.len() {
            if !self.is_same_file(&notes[prev_start], &notes[index]) {
                self.emit_note_chunk(&notes[prev_start..index])?;
                prev_start = index;
            }
        }
        if prev_start < notes.len() {
            self.emit_note_chunk(&notes[prev_start..])?;
        }
        Ok(())
    }

    fn emit_note_chunk(&mut self, notes: &'a [Note<Span>]) -> io::Result<()> {
        let file = self.codemap.find_file(notes[0].span.low());
        let number_width = self.number_width;
        let printer = self.printers
            .entry(file.name())
            .or_insert_with(|| Printer::new(file.clone(), number_width));
        printer.print_notes(self.codemap, notes, &mut self.output)
    }

    fn is_same_file(&self, a: &Note<Span>, b: &Note<Span>) -> bool {
        let file_a = self.codemap.look_up_span(a.span).file;
        let file_b = self.codemap.look_up_span(b.span).file;
        file_a.name() == file_b.name()
    }
}

fn max_line_number(codemap: &CodeMap, diagnostic: &Diagnostic<Span>) -> usize {
    use std::cmp;
    let mut max = 0;
    for note in &diagnostic.notes {
        let loc = codemap.look_up_span(note.span);
        max = cmp::max(max, loc.begin.line + 1);
        max = cmp::max(max, loc.end.line + 1);
    }
    max
}

fn number_length(mut num: usize) -> usize {
    let mut len = 1;
    while num >= 10 {
        len += 1;
        num /= 10;
    }
    len
}

pub fn print_diagnostics<W: Write>(
    codemap: &CodeMap,
    diagnostics: &[Diagnostic<Span>],
    output: W,
) -> io::Result<()> {
    let printer = MultiFilePrinter::new(codemap, output);
    printer.emit_diagnostics(diagnostics)
}
