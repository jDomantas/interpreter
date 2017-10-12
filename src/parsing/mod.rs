mod tokens;
mod lexer;
mod parser;

use std::collections::{BTreeSet, BTreeMap, HashMap};
use codemap::File;
use ast::Name;
use ast::parsed::Module;
use CompileCtx;


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
            "Result" => Ok(modules::RESULT),
            _ => self.inner.get_module_source(name),
        }
    }
}

fn parse_module(file: &File, module: Name, require_def: bool, ctx: &mut CompileCtx) -> Option<Module> {
    let tokens = lexer::lex(file, ctx);
    let module = parser::parse_module(tokens.into_iter(), &module, require_def, ctx);
    module
}

pub(crate) fn parse_modules<T: SourceProvider>(
    main: &str,
    provider: &T,
    ctx: &mut CompileCtx
) -> BTreeMap<Name, Module> {
    let provider = WrappingProvider {
        inner: provider,
        main: main,
    };

    let mut modules = BTreeMap::<Name, Module>::new();
    let mut to_walk = Vec::new();
    let mut checked = BTreeSet::new();

    let main_name = Name::from_string("Main".into());
    let main_file = ctx.codemap.add_file("Main".into(), main.into());
    let main_module = parse_module(&main_file, main_name, false, ctx);

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
                    let file = ctx.codemap.add_file(name.clone(), source.into());
                    let name = Name::from_string(name.clone());
                    let module = parse_module(&file, name, true, ctx);
                    if let Some(module) = module {
                        to_walk.push(module);
                    }
                }
                Err(message) => {
                    ctx.reporter
                        .parse_error(message.as_str(), import.value.name.span)
                        .span_note(message, import.value.name.span)
                        .done();
                }
            }
            checked.insert(name.clone());
        }
        let name = Name::from_string(module.name().into());
        modules.insert(name, module);
    }

    modules
}
