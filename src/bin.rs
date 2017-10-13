extern crate interpreter;

use std::collections::HashMap;
use std::fs;
use std::io::{self, Read};


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
    
    result.diagnostics.emit(io::stderr()).expect("failed to print");

    let mut vm = match result.vm {
        Some(vm) => vm,
        None => return,
    };

    let res = vm.get_main();
    println!("res: {:#?}", res);
}
