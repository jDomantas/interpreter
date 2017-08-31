// TODO: don't forget to remove after developing
#![allow(dead_code)]

pub mod position;
pub mod errors;
pub mod parsing;
pub mod compiler;
pub mod ast;
pub mod vm;

use std::collections::BTreeMap;
use ast::resolved::Sym;
use errors::Errors;
use parsing::SourceProvider;
use vm::{Function, GlobalValue};

pub fn compile<S: SourceProvider>(provider: &S, main: &str)
    -> Result<(BTreeMap<u64, Function>, BTreeMap<Sym, GlobalValue>), Errors>
{
    let mut errors = Errors::new();

    let modules = parsing::parse_modules(main, provider, &mut errors);
    if errors.have_errors() {
        return Err(errors);
    }

    compiler::compile(&modules, &mut errors).map_err(|()| errors)
}
    
