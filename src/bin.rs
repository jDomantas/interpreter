extern crate interpreter;

use std::collections::HashMap;
use std::fs;
use std::io::Read;
use interpreter::position::Span;
use interpreter::diagnostics::Diagnostic;


fn main() {
    let mut file = fs::File::open("test_program.test").unwrap();
    let mut source = String::new();
    file.read_to_string(&mut source).unwrap();
    run(&source);
}

fn run(source: &str) {
    use interpreter::HashMapProvider;

    let modules = HashMapProvider::new(HashMap::new());
    let result = interpreter::compile(&modules, source);
    for diag in result.diagnostics {
        format_error(source, &diag);
    }

    let mut vm = match result.vm {
        Some(vm) => vm,
        None => return,
    };

    let res = vm.get_main();
    println!("res: {:#?}", res);
}

fn format_error(source: &str, error: &Diagnostic<Span>) {
    assert!(!error.notes.is_empty());
    for note in &error.notes {
        if let Some(ref msg) = note.message {
            println!("Error: {}", msg);
        } else {
            println!("No note");
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
