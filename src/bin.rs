extern crate interpreter;

use std::collections::BTreeMap;
use std::fs;
use std::io::Read;
use interpreter::util::position::Span;
use interpreter::util::errors::Error;


fn main() {
    let mut file = fs::File::open("test_program.test").unwrap();
    let mut source = String::new();
    file.read_to_string(&mut source).unwrap();
    run(&source);
}

fn run(source: &str) {
    use interpreter::parsing::BTreeMapProvider;

    let modules = BTreeMapProvider::new(BTreeMap::new());
    let (fns, globals) = match interpreter::compile(&modules, source) {
        Ok(result) => result,
        Err(errors) => {
            for err in errors.into_error_list() {
                format_error(source, &err);
            }
            return;
        }
    };

    // println!("globals: {:#?}", globals);
    // println!("fns: {:#?}", fns);

    let mut vm = interpreter::vm::Vm::new(globals, &fns);
    let res = vm.eval_globals();
    println!("res: {:#?}", res);
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
