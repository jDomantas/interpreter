use std::collections::HashMap;
use ast::Node;
use ast::resolved::{Items, TypeDecl, Type};
use compiler::util::Graph;
use errors::{self, Error};
use position::Span;


fn collect_concrete_types<'a>(type_: &'a Node<Type>, result: &mut Vec<&'a str>) {
    match type_.value {
        Type::SelfType | Type::Var(_) => { }
        Type::Concrete(ref name) => {
            result.push(name);
        }
        Type::Apply(ref a, ref b) |
        Type::Function(ref a, ref b) => {
            collect_concrete_types(a, result);
            collect_concrete_types(b, result);
        }
        Type::Tuple(ref items) => {
            for item in items {
                collect_concrete_types(item, result);
            }
        }
    }
}

fn make_graph<'a, I: Iterator<Item=&'a TypeDecl>>(decls: I) -> Graph<'a, str> {
    let nodes = decls.filter_map(|decl| {
        if let TypeDecl::TypeAlias(ref alias) = *decl {
            let mut depends_on = Vec::new();
            let name = alias.name.value.as_ref();
            if let Some(ref type_) = alias.type_ {
                collect_concrete_types(type_, &mut depends_on);
            }
            Some((name, depends_on))
        } else {
            None
        }
    });
    Graph::new(nodes)
}

pub fn find_alias_cycles(items: &Items) -> Vec<Error> {
    let mut positions = HashMap::<&str, Span>::new();
    let mut err = Vec::new();
    for decl in &items.types {
        if let TypeDecl::TypeAlias(ref alias) = *decl {
            positions.insert(alias.name.value.as_ref(), alias.name.span);
        }
    }
    let graph = make_graph(items.types.iter());
    
    for cycle in graph.find_all_cycles() {
        let message = if cycle.len() == 2 {
            format!("Type alias '{}' depends on itself.", cycle[0])
        } else {
            let main = cycle[0];
            let mut msg = format!("Type alias '{}' depends on itself indirectly. Dependency chain is '{}'",
                main,
                main);
            for typ in &cycle[1..] {
                msg.push_str(" -> '");
                msg.push_str(typ);
                msg.push_str("'");
            }
            msg.push_str(".");
            msg
        };
        let span = positions[cycle[0]];
        let module = errors::symbol_module(cycle[0]);
        err.push(errors::recursive_type_alias(message, span, module));
    }

    err
}
