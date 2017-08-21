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
    use interpreter::compiler::alias_expansion::expand_aliases;
    use interpreter::compiler::precedence::fix_items;
    use interpreter::compiler::kind_check::find_kind_errors;
    use interpreter::compiler::def_grouping::group_items;
    use interpreter::compiler::type_check::infer_types;
    use interpreter::compiler::trait_check::check_items;
    use interpreter::compiler::monomorphisation::monomorphise;

    let modules = interpreter::parsing::HashMapProvider::new(HashMap::new());
    let mut errors = interpreter::errors::Errors::new();

    let modules = parse_modules(source, &modules, &mut errors);
    if errors.have_errors() {
        for err in errors.into_error_list() {
            format_error(source, &err);
        }
        return;
    }

    let items = resolve_symbols(&modules, &mut errors);
    if errors.have_errors() {
        for err in errors.into_error_list() {
            format_error(source, &err);
        }
        return;
    };

    let items = expand_aliases(items, &mut errors);
    if errors.have_errors() {
        for err in errors.into_error_list() {
            format_error(source, &err);
        }
        return;
    };

    let items = fix_items(items, &mut errors);
    if errors.have_errors() {
        for err in errors.into_error_list() {
            format_error(source, &err);
        }
        return;
    };

    let res = find_kind_errors(&items, &mut errors);
    if errors.have_errors() {
        for err in errors.into_error_list() {
            format_error(source, &err);
        }
        return;
    };
    assert!(res.is_ok());

    let items = group_items(items);

    let mut items = infer_types(items, &mut errors);
    if errors.have_errors() {
        for err in errors.into_error_list() {
            format_error(source, &err);
        }
        return;
    }

    check_items(&mut items, &mut errors);
    if errors.have_errors() {
        for err in errors.into_error_list() {
            format_error(source, &err);
        }
        return;
    }

    let mut items = monomorphise(items);

    interpreter::compiler::closure_fix::optimise(&mut items);

    interpreter::ast::monomorphised::printer::print_items(&items);

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
    let _line = if span.start.line == 0 { 0 } else { span.start.line - 1 };
    println!("Error position: {}:{} to {}:{}",
        span.start.line,
        span.start.column,
        span.end.line,
        span.end.column);
    let _ = source;
    /*let line = source.lines().skip(line).next().unwrap();
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
        print!("~");
    }
    println!("");*/
}
