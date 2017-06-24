extern crate interpreter;

use std::fs;
use std::io::Read;
use std::str;
use interpreter::position::Position;

#[test]
fn run_test_programs() {
    let dirs = [
        "./tests/programs/bad/parse"
    ];
    let mut failed_tests = Vec::new();
    let mut passed = 0;
    let mut failed = 0;
    for dir in &dirs {
        let files = fs::read_dir(dir).unwrap();
        for file in files {
            let file = file.unwrap();
            let name = format!("{}/{}", dir, file.file_name().to_str().unwrap());
            let mut file = fs::File::open(&name).unwrap();
            let mut source = String::new();
            file.read_to_string(&mut source).unwrap();
            let is_ok = run_test(&source);
            let outcome = if is_ok { "ok" } else { "FAIL" };
            println!("program: \"{}\" ... {}", name, outcome);
            if is_ok {
                passed += 1;
            } else {
                failed_tests.push(name);
                failed += 1;
            }
        }
    }

    println!("{} passed, {} failed", passed, failed);
    if !failed_tests.is_empty() {
        println!("failed tests:");
        for test in &failed_tests {
            println!("- {}", test);
        }
        panic!("{} passed, {} failed", passed, failed);
    }
}

fn run_test(source: &str) -> bool {
    let expected = parse_expected_outcome(source);
    let outcome = run_program(source);
    println!("outcome: {:?}", outcome);
    outcome == expected
}

fn run_program(source: &str) -> Outcome {
    let (_module, mut errors) = interpreter::parsing::parse_module(source, "Main", false);
    if !errors.is_empty() {
        errors.sort_by(interpreter::errors::Error::ordering);
        let pos = errors[0].notes[0].span.start;
        return Outcome::ParseError(pos);
    }

    Outcome::Ok
}

fn parse_expected_outcome(source: &str) -> Outcome {
    for line in source.lines() {
        if line.starts_with("-- expect parse error: line ") {
            let segments = line.split(" ").collect::<Vec<_>>();
            let line = str::parse::<usize>(segments[5]).unwrap();
            let column = str::parse::<usize>(segments[7]).unwrap();
            return Outcome::ParseError(Position::new(line, column));
        } 
    }

    panic!("no expectations in program");
}

#[derive(PartialEq, Eq, Debug)]
enum Outcome {
    ParseError(Position),
    Ok,
}
