extern crate interpreter;
extern crate walkdir;

use std::fs;
use std::io::Read;
use std::str;
use std::collections::HashMap;
use std::path::Path;
use interpreter::HashMapProvider;
use interpreter::diagnostics::Pos;
use interpreter::vm::{self, EvalError};


#[test]
fn run_test_programs() {
    let mut runner = TestRunner::new();
    for entry in walkdir::WalkDir::new("./tests/programs") {
        let entry = entry.expect("failed to read fs entry");
        if entry.file_type().is_file() {
            let name = format!("{}", entry.path().display());
            runner.run_file(entry.path(), &name);
        }
    }
    runner.report_results();
}

struct TestRunner {
    failed: u64,
    passed: u64,
    results: HashMap<String, TestResult>,
}

impl TestRunner {
    fn new() -> TestRunner {
        TestRunner {
            failed: 0,
            passed: 0,
            results: HashMap::new(),
        }
    }

    fn run_file(&mut self, path: &Path, name: &str) {
        let mut file = fs::File::open(path).expect("failed to open test file");
        let mut source = String::new();
        file.read_to_string(&mut source).expect("failed to read test file");
        self.run_test(name, &source);
    }

    fn run_test(&mut self, name: &str, source: &str) {
        let result = run_test(source);
        match result {
            TestResult::Ok => self.passed += 1,
            TestResult::Err(_, _) => self.failed += 1,
        }
        self.results.insert(name.into(), result);
    }

    fn report_results(&self) {
        let mut failed_tests = String::new();
        for (name, result) in &self.results {
            match *result {
                TestResult::Ok => {
                    println!("test {} ... ok", name);
                }
                TestResult::Err(ref expected, ref outcome) => {
                    println!("test {} ... FAILED", name);
                    println!("expected:");
                    expected.report();
                    println!("outcome:");
                    outcome.report();
                    failed_tests.push_str("'");
                    failed_tests.push_str(name);
                    failed_tests.push_str("', ");
                }
            }
        }
        println!("passed: {}, failed: {}", self.passed, self.failed);
        if self.failed > 0 {
            panic!("failed {} tests: {}", self.failed, failed_tests);
        }
    }
}

fn run_test(source: &str) -> TestResult {
    let expected = parse_expectation(source);
    let outcome = run_program(source);
    TestResult::from_expectation_and_outcome(expected, outcome)
}

fn run_program(source: &str) -> Outcome {
    use interpreter::SourceProvider;
    use interpreter::diagnostics::Phase;

    let modules = parse_modules_from_source(source);
    let main = modules.get_module_source("Main").unwrap();

    let result = interpreter::compile(&modules, main);
    if let Some(mut vm) = result.vm {
        match vm.get_main() {
            Ok(value) => Outcome::Ok(value),
            Err(err) => Outcome::EvalError(err),
        }
    } else {
        let mut errors = result.diagnostics;
        errors.sort_by(interpreter::diagnostics::Diagnostic::ordering);
        let err = errors[0].clone();
        let pos = err.primary_span.unwrap().start;
        let message = err.message.clone();
        match err.phase {
            Phase::Parsing => Outcome::ParseError(shift_pos(pos), message),
            Phase::SymbolResolution => {
                let errors = errors
                    .into_iter()
                    .map(|e| {
                        assert_eq!(e.phase, Phase::SymbolResolution);
                        (shift_pos(e.primary_span.unwrap().start), e.message.clone())
                    })
                    .collect();
                Outcome::SymbolError(errors)
            }
            Phase::TypeAliasExpansion => {
                let errors = errors
                    .into_iter()
                    .map(|e| {
                        assert_eq!(e.phase, Phase::TypeAliasExpansion);
                        (shift_pos(e.primary_span.unwrap().start), e.message)
                    })
                    .collect();
                Outcome::AliasError(errors)
            }
            Phase::FixityResolution => Outcome::FixityError(shift_pos(pos), message),
            Phase::KindChecking => Outcome::KindError(shift_pos(pos), message),
            Phase::TypeChecking => Outcome::TypeError(shift_pos(pos), message),
            Phase::TraitChecking => Outcome::TraitError(shift_pos(pos), message),
            Phase::PatternError => unimplemented!(),
        }
    }
}

fn shift_pos(pos: Pos) -> Pos {
    let Pos { line, col } = pos;
    Pos {
        line: line + 1,
        col: col + 1,
    }
}

fn parse_expectation(source: &str) -> Expectation {
    fn single_pos_error<F>(source: &str, kind: &str, builder: F) -> Option<Expectation>
        where F: FnOnce(Pos) -> Expectation
    {
        for line in source.lines() {
            let parts = line.split(' ').collect::<Vec<_>>();
            if line.len() >= 8 &&
                parts[0] == "--" &&
                parts[1] == "expect" &&
                parts[2] == kind &&
                parts[3] == "error:"
            {
                let line = str::parse::<usize>(parts[5]).unwrap();
                let col = str::parse::<usize>(parts[7]).unwrap();
                return Some(builder(Pos { line, col }));
            }
        }
        None
    }

    if let Some(e) = single_pos_error(source, "parse", Expectation::ParseError) {
        return e;
    }

    for line in source.lines() {
        if line.starts_with("-- expect symbol errors: ") {
            let mut positions = Vec::new();
            for pos in line.split(' ').skip(4) {
                let parts = pos.split(':').collect::<Vec<_>>();
                let line = str::parse::<usize>(parts[0]).unwrap();
                let col = str::parse::<usize>(parts[1]).unwrap();
                positions.push(Pos { line, col });
            }
            return Expectation::SymbolError(positions);
        } 
    }

    for line in source.lines() {
        if line.starts_with("-- expect recursive alias error") {
            return Expectation::RecursiveAliasError;
        } 
    }

    if let Some(e) = single_pos_error(source, "fixity", Expectation::FixityError) {
        return e;
    }

    if let Some(e) = single_pos_error(source, "kind", Expectation::KindError) {
        return e;
    }

    if let Some(e) = single_pos_error(source, "type", Expectation::TypeError) {
        return e;
    }

    if let Some(e) = single_pos_error(source, "trait", Expectation::TraitError) {
        return e;
    }

    for line in source.lines() {
        if line.starts_with("-- expect int: ") {
            let value = str::parse::<i64>(&line[15..]).unwrap();
            return Expectation::Ok(Value::Int(value));
        } else if line.starts_with("-- expect bool: true") {
            return Expectation::Ok(Value::Bool(true));
        } else if line.starts_with("-- expect bool: false") {
            return Expectation::Ok(Value::Bool(false));
        } else if line.starts_with("-- expect string: ") {
            let s = line[18..].to_string();
            return Expectation::Ok(Value::Str(s));
        }
    }

    panic!("no expectations in program");
}

#[derive(Debug)]
enum Value {
    Bool(bool),
    Int(i64),
    Str(String),
}

impl Value {
    fn matches_vm_val(&self, value: &vm::Value) -> bool {
        match (self, value) {
            (&Value::Bool(b), &vm::Value::Bool(b2)) => b == b2,
            (&Value::Int(i), &vm::Value::Int(i2)) => i == i2,
            (&Value::Str(ref s), &vm::Value::Str(ref s2)) => *s == **s2,
            _ => false,
        }
    }
}

enum Expectation {
    ParseError(Pos),
    SymbolError(Vec<Pos>),
    RecursiveAliasError,
    FixityError(Pos),
    KindError(Pos),
    TypeError(Pos),
    TraitError(Pos),
    Ok(Value),
}

impl Expectation {
    fn report(&self) {
        match *self {
            Expectation::ParseError(pos) => {
                println!("parse error at line {}, column {}", pos.line, pos.col);
            }
            Expectation::SymbolError(ref pos) => {
                println!("symbol errors at:");
                for pos in pos {
                    println!("line {}, column {}", pos.line, pos.col);
                }
            }
            Expectation::RecursiveAliasError => {
                println!("recursive alias error");
            }
            Expectation::FixityError(pos) => {
                println!("fixity error at line {}, column {}", pos.line, pos.col);
            }
            Expectation::KindError(pos) => {
                println!("kind error at line {}, column {}", pos.line, pos.col);
            }
            Expectation::TypeError(pos) => {
                println!("type error at line {}, column {}", pos.line, pos.col);
            }
            Expectation::TraitError(pos) => {
                println!("trait error at line {}, column {}", pos.line, pos.col);
            }
            Expectation::Ok(ref value) => {
                println!("success: {:?}", value);
            }
        }
    }
}

enum Outcome {
    ParseError(Pos, String),
    SymbolError(Vec<(Pos, String)>),
    AliasError(Vec<(Pos, String)>),
    FixityError(Pos, String),
    KindError(Pos, String),
    TypeError(Pos, String),
    TraitError(Pos, String),
    EvalError(EvalError),
    Ok(vm::Value),
}

impl Outcome {
    fn report(&self) {
        match *self {
            Outcome::ParseError(pos, ref msg) => {
                println!("parse error at line {}, column {}", pos.line, pos.col);
                println!("reason: {}", msg);
            }
            Outcome::SymbolError(ref pos) => {
                println!("symbol errors:");
                for &(pos, ref msg) in pos {
                    println!("line {}, column {}, reason: {}",
                        pos.line,
                        pos.col,
                        msg);
                }
            }
            Outcome::AliasError(ref pos) => {
                println!("type alias errors:");
                for &(pos, ref msg) in pos {
                    println!("line {}, column {}, reason: {}",
                        pos.line,
                        pos.col,
                        msg);
                }
            }
            Outcome::FixityError(pos, ref msg) => {
                println!("fixity error at line {}, column {}", pos.line, pos.col);
                println!("reason: {}", msg);
            }
            Outcome::KindError(pos, ref msg) => {
                println!("kind error at line {}, column {}", pos.line, pos.col);
                println!("reason: {}", msg);
            }
            Outcome::TypeError(pos, ref msg) => {
                println!("type error at line {}, column {}", pos.line, pos.col);
                println!("reason: {}", msg);
            }
            Outcome::TraitError(pos, ref msg) => {
                println!("trait error at line {}, column {}", pos.line, pos.col);
                println!("reason: {}", msg);
            }
            Outcome::EvalError(ref err) => {
                println!("eval error: {:?}", err);
            }
            Outcome::Ok(ref value) => {
                println!("success: {:?}", value);
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
            (&Expectation::ParseError(pos), &Outcome::ParseError(pos2, _)) |
            (&Expectation::FixityError(pos), &Outcome::FixityError(pos2, _)) |
            (&Expectation::KindError(pos), &Outcome::KindError(pos2, _)) |
            (&Expectation::TypeError(pos), &Outcome::TypeError(pos2, _)) |
            (&Expectation::TraitError(pos), &Outcome::TraitError(pos2, _)) => {
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
            (&Expectation::RecursiveAliasError, &Outcome::AliasError(_)) => {
                return TestResult::Ok;
            }
            (&Expectation::Ok(ref val1), &Outcome::Ok(ref val2)) => {
                if val1.matches_vm_val(val2) {
                    return TestResult::Ok;
                }
            }
            _ => { }
        }

        TestResult::Err(expected, outcome)
    }
}

fn parse_modules_from_source(source: &str) -> HashMapProvider {
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
    HashMapProvider::new(modules)
}
