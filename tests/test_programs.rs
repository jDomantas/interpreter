extern crate interpreter;

use std::fs;
use std::io::Read;
use std::str;
use std::collections::BTreeMap;
use interpreter::parsing::BTreeMapProvider;
use interpreter::position::Position;

#[test]
fn run_test_programs() {
    let dirs = [
        "./tests/programs/bad/parse",
        "./tests/programs/bad/symbol",
        "./tests/programs/bad/fixity",
        "./tests/programs/bad/type_alias",
        "./tests/programs/bad/kind",
        "./tests/programs/bad/type",
        "./tests/programs/bad/trait",
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
                    println!("program: \"{}\" ... FAIL", name);
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
    use interpreter::parsing::SourceProvider;
    use interpreter::errors::Phase;

    let modules = parse_modules_from_source(source);
    let main = modules.get_module_source("Main").unwrap();

    match interpreter::compile(&modules, main) {
        Ok(_) => Outcome::Ok,
        Err(errors) => {
            let errors = errors.into_error_list();
            let err = errors[0].clone();
            let pos = err.notes[0].span.start;
            let message = err.notes[0].message.clone();
            match err.phase {
                Phase::Parsing => Outcome::ParseError(pos, message),
                Phase::SymbolResolution => {
                    let errors = errors
                        .into_iter()
                        .map(|e| {
                            assert_eq!(e.phase, Phase::SymbolResolution);
                            (e.notes[0].span.start, e.notes[0].message.clone())
                        })
                        .collect();
                    Outcome::SymbolError(errors)
                }
                Phase::TypeAliasExpansion => {
                    let errors = errors
                        .into_iter()
                        .map(|e| {
                            assert_eq!(e.phase, Phase::TypeAliasExpansion);
                            (e.notes[0].span.start, e.notes[0].message.clone())
                        })
                        .collect();
                    Outcome::AliasError(errors)
                }
                Phase::FixityResolution => Outcome::FixityError(pos, message),
                Phase::KindChecking => Outcome::KindError(pos, message),
                Phase::TypeChecking => Outcome::TypeError(pos, message),
                Phase::TraitChecking => Outcome::TraitError(pos, message),
                Phase::PatternError => unimplemented!(),
            }
        }
    }
}

fn parse_expectation(source: &str) -> Expectation {
    fn single_pos_error<F>(source: &str, kind: &str, builder: F) -> Option<Expectation>
        where F: FnOnce(Position) -> Expectation
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
                let column = str::parse::<usize>(parts[7]).unwrap();
                return Some(builder(Position::new(line, column)));
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
                let column = str::parse::<usize>(parts[1]).unwrap();
                positions.push(Position::new(line, column));
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

    panic!("no expectations in program");
}

enum Expectation {
    ParseError(Position),
    SymbolError(Vec<Position>),
    RecursiveAliasError,
    FixityError(Position),
    KindError(Position),
    TypeError(Position),
    TraitError(Position),
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
            Expectation::RecursiveAliasError => {
                println!("recursive alias error");
            }
            Expectation::FixityError(pos) => {
                println!("fixity error at line {}, column {}", pos.line, pos.column);
            }
            Expectation::KindError(pos) => {
                println!("kind error at line {}, column {}", pos.line, pos.column);
            }
            Expectation::TypeError(pos) => {
                println!("type error at line {}, column {}", pos.line, pos.column);
            }
            Expectation::TraitError(pos) => {
                println!("trait error at line {}, column {}", pos.line, pos.column);
            }
        }
    }
}

enum Outcome {
    ParseError(Position, String),
    SymbolError(Vec<(Position, String)>),
    AliasError(Vec<(Position, String)>),
    FixityError(Position, String),
    KindError(Position, String),
    TypeError(Position, String),
    TraitError(Position, String),
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
            Outcome::AliasError(ref pos) => {
                println!("type alias errors:");
                for &(pos, ref msg) in pos {
                    println!("line {}, column {}, reason: {}",
                        pos.line,
                        pos.column,
                        msg);
                }
            }
            Outcome::FixityError(pos, ref msg) => {
                println!("fixity error at line {}, column {}", pos.line, pos.column);
                println!("reason: {}", msg);
            }
            Outcome::KindError(pos, ref msg) => {
                println!("kind error at line {}, column {}", pos.line, pos.column);
                println!("reason: {}", msg);
            }
            Outcome::TypeError(pos, ref msg) => {
                println!("type error at line {}, column {}", pos.line, pos.column);
                println!("reason: {}", msg);
            }
            Outcome::TraitError(pos, ref msg) => {
                println!("trait error at line {}, column {}", pos.line, pos.column);
                println!("reason: {}", msg);
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
            _ => { }
        }

        TestResult::Err(expected, outcome)
    }
}

fn parse_modules_from_source(source: &str) -> BTreeMapProvider {
    let mut modules = BTreeMap::new();
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
    BTreeMapProvider::new(modules)
}
