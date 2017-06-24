extern crate interpreter;

use std::fs;
use std::io::Read;
use std::str;
use std::collections::HashMap;
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
            let result = run_test(&source);            
            match result {
                TestResult::Ok => {
                    println!("program: \"{}\" ... ok", name);
                    passed += 1;
                }
                TestResult::Err(expectation, outcome) => {
                    failed_tests.push((name, expectation, outcome));
                    failed += 1;
                }
            }
        }
    }

    println!("{} passed, {} failed", passed, failed);
    if !failed_tests.is_empty() {
        println!("failed tests:");
        for (test, expectation, outcome) in failed_tests {
            println!("- {}", test);
            println!("expected:");
            expectation.report();
            println!("outcome:");
            outcome.report();
        }
        panic!("{} passed, {} failed", passed, failed);
    }
}

fn run_test(source: &str) -> TestResult {
    let expected = parse_expectation(source);
    let outcome = run_program(source);
    TestResult::from_expectation_and_outcome(expected, outcome)
}

fn run_program(source: &str) -> Outcome {
    use interpreter::parsing::{parse_modules, SourceProvider};
    use interpreter::compiler::symbols::resolve_symbols;

    let modules = Modules::from_source(source);
    let main = modules.get_module_source("Main").unwrap();

    let (modules, mut errors) = parse_modules(&main, modules);
    if !errors.is_empty() {
        errors.sort_by(interpreter::errors::Error::ordering);
        let pos = errors[0].notes[0].span.start;
        let message = errors[0].notes[0].message.clone();
        return Outcome::ParseError(pos, message);
    }

    match resolve_symbols(modules) {
        Err(mut errors) => {
            assert!(!errors.is_empty());
            errors.sort_by(interpreter::errors::Error::ordering);
            let mut simple_errors = Vec::new();
            for err in errors {
                let pos = err.notes[0].span.start;
                let message = err.notes[0].message.clone();
                simple_errors.push((pos, message));
            }
            return Outcome::SymbolError(simple_errors);
        }
        Ok(_) => { }
    }

    Outcome::Ok
}

fn parse_expectation(source: &str) -> Expectation {
    for line in source.lines() {
        if line.starts_with("-- expect parse error: line ") {
            let parts = line.split(" ").collect::<Vec<_>>();
            let line = str::parse::<usize>(parts[5]).unwrap();
            let column = str::parse::<usize>(parts[7]).unwrap();
            return Expectation::ParseError(Position::new(line, column));
        } 
    }

    for line in source.lines() {
        if line.starts_with("-- expect symbol errors: ") {
            let mut positions = Vec::new();
            for pos in line.split(" ").skip(4) {
                let parts = pos.split(":").collect::<Vec<_>>();
                let line = str::parse::<usize>(parts[0]).unwrap();
                let column = str::parse::<usize>(parts[1]).unwrap();
                positions.push(Position::new(line, column));
            }
            return Expectation::SymbolError(positions);
        } 
    }

    panic!("no expectations in program");
}

enum Expectation {
    ParseError(Position),
    SymbolError(Vec<Position>),
}

impl Expectation {
    fn report(&self) {
        match *self {
            Expectation::ParseError(pos) => {
                println!("parse error at line {}, column {}", pos.line, pos.column);
            }
            Expectation::SymbolError(ref pos) => {
                println!("symbol errors at:");
                for pos in pos {
                    println!("line {}, column {}", pos.line, pos.column);
                }
            }
        }
    }
}

enum Outcome {
    ParseError(Position, String),
    SymbolError(Vec<(Position, String)>),
    Ok,
}

impl Outcome {
    fn report(&self) {
        match *self {
            Outcome::ParseError(pos, ref msg) => {
                println!("parse error at line {}, column {}", pos.line, pos.column);
                println!("reason: {}", msg);
            }
            Outcome::SymbolError(ref pos) => {
                println!("symbol errors:");
                for &(pos, ref msg) in pos {
                    println!("line {}, column {}, reason: {}",
                        pos.line,
                        pos.column,
                        msg);
                }
            }
            Outcome::Ok => {
                println!("program passed");
            }
        }
    }
}

enum TestResult {
    Ok,
    Err(Expectation, Outcome),
}

impl TestResult {
    fn from_expectation_and_outcome(expected: Expectation, outcome: Outcome) -> TestResult {
        match (&expected, &outcome) {
            (&Expectation::ParseError(pos), &Outcome::ParseError(pos2, _)) => {
                if pos == pos2 {
                    return TestResult::Ok;
                }
            }
            (&Expectation::SymbolError(ref pos), &Outcome::SymbolError(ref pos2)) => {
                if pos.len() == pos2.len() {
                    let mut all_good = true;
                    for (p, &(p2, _)) in pos.iter().zip(pos2.iter()) {
                        if *p != p2 {
                            all_good = false;
                        }
                    }
                    if all_good {
                        return TestResult::Ok;
                    }
                }
            }
            _ => { }
        }

        TestResult::Err(expected, outcome)
    }
}

struct Modules(HashMap<String, String>);

impl Modules {
    fn from_source(source: &str) -> Modules {
        let mut modules = HashMap::new();
        let mut current_module = String::new();
        let mut module_name = "Main".to_string();
        for line in source.lines() {
            if line.starts_with("-- module: ") {
                modules.insert(module_name, current_module);
                module_name = line.chars().skip(11).collect();
                current_module = String::new();
            } else {
                current_module.push_str(line);
                current_module.push('\n');
            }
        }
        modules.insert(module_name, current_module);
        Modules(modules)
    }
}

impl interpreter::parsing::SourceProvider for Modules {
    fn get_module_source(&self, name: &str) -> Result<String, String> {
        match self.0.get(name) {
            Some(source) => Ok(source.clone()),
            None => Err(format!("module unavailable: {}", name)),
        }
    }
}
