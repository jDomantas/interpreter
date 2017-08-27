mod tokens;
mod lexer;
mod parser;

use std::collections::{BTreeSet, BTreeMap};
use ast::Name;
use ast::parsed::Module;
use errors::Errors;

pub trait SourceProvider {
    fn get_module_source(&self, name: &str) -> Result<&str, String>;
}

pub struct BTreeMapProvider(BTreeMap<String, String>);

impl BTreeMapProvider {
    pub fn new(modules: BTreeMap<String, String>) -> Self {
        BTreeMapProvider(modules)
    }
}

impl SourceProvider for BTreeMapProvider {
    fn get_module_source(&self, name: &str) -> Result<&str, String> {
        match self.0.get(name) {
            Some(source) => Ok(source),
            None => Err(format!("module unavailable: {}", name)),
        }
    }
}

struct WrappingProvider<'a, 'b, T: 'a> {
    inner: &'a T,
    main: &'b str,
}

impl<'a, 'b, T: 'a + SourceProvider> SourceProvider for WrappingProvider<'a, 'b, T> {
    fn get_module_source(&self, name: &str) -> Result<&str, String> {
        use compiler::builtins::modules;
        match name {
            "Main" => Ok(self.main),
            "Basics" => Ok(modules::BASICS),
            "Option" => Ok(modules::OPTION),
            "List" => Ok(modules::LIST),
            "String" => Ok(modules::STRING),
            "Computation" => Ok(modules::COMPUTATION),
            _ => self.inner.get_module_source(name),
        }
    }
}

pub fn parse_module(source: &str, module: Name, require_def: bool, errors: &mut Errors) -> Option<Module> {
    let tokens = lexer::lex(source, module.clone(), errors);
    let module = parser::parse_module(tokens.into_iter(), module, require_def, errors);
    module
}

pub fn parse_modules<T: SourceProvider>(main: &str, provider: &T, errors: &mut Errors) -> BTreeMap<Name, Module> {
    let provider = WrappingProvider {
        inner: provider,
        main: main,
    };
    
    let mut modules = BTreeMap::<Name, Module>::new();
    let mut to_walk = Vec::new();
    let mut checked = BTreeSet::new();

    let main_name = Name::from_string("Main".into());
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
