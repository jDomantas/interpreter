mod tokens;
mod lexer;
mod parser;

use std::collections::{HashSet, HashMap};
use ast::Name;
use ast::parsed::Module;
use errors::Errors;

pub trait SourceProvider {
    fn get_module_source(&self, name: &str) -> Result<&str, String>;
}

pub struct HashMapProvider(HashMap<String, String>);

impl HashMapProvider {
    pub fn new(modules: HashMap<String, String>) -> Self {
        HashMapProvider(modules)
    }
}

impl SourceProvider for HashMapProvider {
    fn get_module_source(&self, name: &str) -> Result<&str, String> {
        match self.0.get(name) {
            Some(source) => Ok(source),
            None => Err(format!("module unavailable: {}", name)),
        }
    }
}

pub fn parse_module(source: &str, module: Name, require_def: bool, errors: &mut Errors) -> Option<Module> {
    let tokens = lexer::lex(source, module.clone(), errors);
    let module = parser::parse_module(tokens.into_iter(), module, require_def, errors);
    module
}

pub fn parse_modules<T: SourceProvider>(main: &str, provider: &T, errors: &mut Errors) -> HashMap<Name, Module> {
    let mut modules = HashMap::<Name, Module>::new();
    let mut to_walk = Vec::new();
    let mut checked = HashSet::new();

    let main_name = Name::from_string("<main>".into());
    let main_module = parse_module(main, main_name, false, errors);

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
                    let name = Name::from_string(name.clone());
                    let module = parse_module(source, name, true, errors);
                    if let Some(module) = module {
                        to_walk.push(module);
                    }
                }
                Err(message) => {
                    errors
                        .parse_error(&Name::from_string(module.name().into()))
                        .note(message, import.value.name.span)
                        .done();
                }
            }
            checked.insert(name.clone());
        }
        let name = Name::from_string(module.def.value.name.value.clone());
        modules.insert(name, module);
    }

    modules
}
