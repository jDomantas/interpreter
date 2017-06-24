mod tokens;
mod lexer;
mod parser;

use std::collections::{HashSet, HashMap};
use ast::parsed::Module;
use errors::{self, Error};

pub trait SourceProvider {
    fn get_module_source(&self, name: &str) -> Result<String, String>;
}

pub fn parse_module(source: &str, module: &str, require_def: bool) -> (Option<Module>, Vec<Error>) {
    let (tokens, lex_errors) = lexer::lex(source, module);
    let (module, mut parse_errors) = parser::parse_module(tokens.into_iter(), module, require_def);
    parse_errors.extend(lex_errors);
    (module, parse_errors)
}

pub fn parse_modules<T: SourceProvider>(main: &str, provider: T) -> (HashMap<String, Module>, Vec<Error>) {
    let mut modules = HashMap::<String, Module>::new();
    let mut to_walk = Vec::new();
    let mut checked = HashSet::new();

    let (main_module, mut errors) = parse_module(main, "<main>", false);

    if let Some(module) = main_module {
        to_walk.push(module);
    }

    while let Some(module) = to_walk.pop() {
        for import in &module.imports {
            let name = &import.value.name.value;
            if checked.contains(name) {
                continue;
            }
            match provider.get_module_source(name) {
                Ok(source) => {
                    let (module, parse_errors) = parse_module(&source, &name, true);
                    errors.extend(parse_errors);
                    if let Some(module) = module {
                        to_walk.push(module);
                    }
                }
                Err(message) => {
                    let error = errors::module_not_loaded(
                        message,
                        import.value.name.span,
                        name.as_ref());
                    errors.push(error);
                }
            }
            checked.insert(name.clone());
        }
        let name = module.def.value.name.value.clone();
        modules.insert(name, module);
    }

    (modules, errors)
}
