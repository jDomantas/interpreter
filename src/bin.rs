extern crate interpreter;

use std::collections::HashMap;
use std::fs;
use std::io::Read;
use interpreter::position::Span;
use interpreter::errors::Error;


fn main() {
    let mut file = fs::File::open("test_program.test").unwrap();
    let mut source = String::new();
    file.read_to_string(&mut source).unwrap();
    run(&source);
}

fn run(source: &str) {
    use interpreter::parsing::parse_modules;
    use interpreter::compiler::symbols::resolve_symbols;
    use interpreter::compiler::alias_expansion::find_alias_cycles;
    use interpreter::compiler::precedence::fix_items;
    use interpreter::compiler::kind_check::find_kind_errors;

    let modules = interpreter::parsing::HashMapProvider::new(HashMap::new());

    let (modules, mut errors) = parse_modules(source, &modules);
    if !errors.is_empty() {
        errors.sort_by(interpreter::errors::Error::ordering);
        for err in errors {
            format_error(source, &err);
        }
        return;
    }

    let items = match resolve_symbols(&modules) {
        Err(mut errors) => {
            assert!(!errors.is_empty());
            errors.sort_by(interpreter::errors::Error::ordering);
            for err in errors {
                format_error(source, &err);
            }
            println!("here");
            return;
        }
        Ok(items) => items,
    };

    let mut alias_errors = find_alias_cycles(&items);
    if !alias_errors.is_empty() {
        alias_errors.sort_by(interpreter::errors::Error::ordering);
        for err in alias_errors {
            format_error(source, &err);
        }
        return;
    }

    let (items, mut fixity_errors) = fix_items(items);
    if !fixity_errors.is_empty() {
        fixity_errors.sort_by(interpreter::errors::Error::ordering);
        for err in fixity_errors {
            format_error(source, &err);
        }
        return;
    }

    let mut kind_errors = find_kind_errors(&items);
    if !kind_errors.is_empty() {
        kind_errors.sort_by(interpreter::errors::Error::ordering);
        for err in kind_errors {
            format_error(source, &err);
        }
        return;
    }

    println!("OK");
}

fn format_error(source: &str, error: &Error) {
    assert!(!error.notes.is_empty());
    let mut first = true;
    for note in &error.notes {
        if first {
            println!("Error: {}", note.message);
            first = false;
        } else {
            println!("Note: {}", note.message);
        }
        display_span(source, note.span);
    }
    println!("");
}

fn display_span(source: &str, span: Span) {
    let line = source.lines().skip(span.start.line - 1).next().unwrap();
    println!("{: >5} | {}", span.start.line, line);
    for _ in 0..(span.start.column + 7) {
        print!(" ");
    }
    let end_col = if span.start.line == span.end.line {
        span.end.column
    } else {
        line.trim_right().chars().count() + 1
    };
    for _ in (span.start.column)..(end_col + 1) {
        print!("^");
    }
    println!("");
}
