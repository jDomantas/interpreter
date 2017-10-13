mod tokens;
mod lexer;
mod parser;

use std::collections::{BTreeSet, BTreeMap, HashMap};
use std::fmt::Display;
use codemap::File;
use ast::parsed::Module;
use CompileCtx;


pub trait SourceProvider {
    type Error: Display;
    fn get_module_source(&mut self, name: &str) -> Result<String, Self::Error>;
}

pub struct HashMapProvider(HashMap<String, String>);

impl HashMapProvider {
    pub fn new(modules: HashMap<String, String>) -> Self {
        HashMapProvider(modules)
    }
}

impl SourceProvider for HashMapProvider {
    type Error = String;

    fn get_module_source(&mut self, name: &str) -> Result<String, Self::Error> {
        match self.0.get(name).cloned() {
            Some(source) => Ok(source),
            None => Err(format!("module `{}` is unavailable", name)),
        }
    }
}

struct WrappingProvider<'a, T: 'a> {
    inner: &'a mut T,
    main: &'a str,
}

impl<'a, T: 'a + SourceProvider> SourceProvider for WrappingProvider<'a, T> {
    type Error = T::Error;

    fn get_module_source(&mut self, name: &str) -> Result<String, T::Error> {
        use compiler::builtins::modules;
        match name {
            "Main" => Ok(self.main.into()),
            "Basics" => Ok(modules::BASICS.into()),
            "Option" => Ok(modules::OPTION.into()),
            "List" => Ok(modules::LIST.into()),
            "String" => Ok(modules::STRING.into()),
            "Computation" => Ok(modules::COMPUTATION.into()),
            "Result" => Ok(modules::RESULT.into()),
            _ => self.inner.get_module_source(name),
        }
    }
}

fn parse_module(
    file: &File,
    module: &str,
    require_def: bool,
    ctx: &mut CompileCtx,
) -> Option<Module> {
    let tokens = lexer::lex(file, ctx);
    let module = parser::parse_module(tokens.into_iter(), module, require_def, ctx);
    module
}

pub(crate) fn parse_modules<T: SourceProvider>(
    main: &str,
    provider: &mut T,
    ctx: &mut CompileCtx
) -> BTreeMap<String, Module> {
    let mut provider = WrappingProvider {
        inner: provider,
        main: main,
    };

    let mut modules = BTreeMap::<String, Module>::new();
    let mut to_walk = Vec::new();
    let mut checked = BTreeSet::new();

    let main_file = ctx.codemap.add_file("Main".into(), main.into());
    let main_module = parse_module(&main_file, "Main", false, ctx);

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
                    let file = ctx.codemap.add_file(name.clone(), source);
                    let module = parse_module(&file, name, true, ctx);
                    if let Some(module) = module {
                        to_walk.push(module);
                    }
                }
                Err(err) => {
                    let message = err.to_string();
                    ctx.reporter
                        .parse_error(message.as_str(), import.value.name.span)
                        .span_note(message, import.value.name.span)
                        .done();
                }
            }
            checked.insert(name.clone());
        }
        modules.insert(module.name().into(), module);
    }

    modules
}
