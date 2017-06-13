mod tokens;
mod lexer;
mod parser;

use std::collections::{HashSet, HashMap};
use ast::{Module, RawSymbol};
use errors::ParseError;

pub trait SourceProvider {
    fn get_module_source(&self, name: &str) -> Result<String, String>;
}

pub fn parse_module(source: &str, require_def: bool) -> (Option<Module<RawSymbol>>, Vec<ParseError>) {
    let (tokens, lex_errors) = lexer::lex(source);
    let (module, mut parse_errors) = parser::parse_module(tokens.into_iter(), require_def);
    parse_errors.extend(lex_errors);
    parse_errors.sort_by(|a, b| a.span.start.cmp(&b.span.start));
    (module, parse_errors)
}

pub fn parse_modules<T: SourceProvider>(main: &str, provider: T) -> HashMap<String, Module<RawSymbol>> {
    let mut modules = HashMap::<String, Module<RawSymbol>>::new();
    let mut to_walk = Vec::new();
    let mut checked = HashSet::new();

    let (main_module, mut errors) = parse_module(main, false);

    if let Some(module) = main_module {
        to_walk.push(module);
    }

    while let Some(module) = to_walk.pop() {
        for import in &module.imports {
            let name = &import.node.name.node;
            if checked.contains(name) {
                continue;
            }
            match provider.get_module_source(name) {
                Ok(source) => {
                    let (module, parse_errors) = parse_module(&source, true);
                    // TODO: mark from which file each error was
                    errors.extend(parse_errors);
                    if let Some(module) = module {
                        to_walk.push(module);
                    }
                }
                Err(_message) => {
                    // TODO: module was not found, report error
                    unimplemented!()
                }
            }
            checked.insert(name.clone());
        }
        let name = module.def.node.name.node.clone();
        modules.insert(name, module);
    }

    unimplemented!()
}
