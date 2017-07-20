use std::collections::HashMap;
use ast::{Node, Name};
use ast::resolved::{Items, TypeDecl, Type, TypeAnnot, Sym, Symbol};
use compiler::util::{self, Graph};
use errors::Errors;
use position::{Span, DUMMY_SPAN};


fn make_graph<'a, I: Iterator<Item=&'a TypeDecl>>(decls: I) -> Graph<'a, Sym> {
    let nodes = decls.filter_map(|decl| {
        if let TypeDecl::TypeAlias(ref alias) = *decl {
            let mut depends_on = Vec::new();
            if let Some(ref type_) = alias.type_ {
                util::collect_concrete_types(type_, &mut depends_on);
            }
            Some((&alias.name.value, depends_on))
        } else {
            None
        }
    });
    Graph::new(nodes)
}

pub fn find_alias_cycles(items: &Items, errors: &mut Errors) -> Result<(), ()> {
    let mut positions = HashMap::new();
    for decl in &items.types {
        if let TypeDecl::TypeAlias(ref alias) = *decl {
            positions.insert(alias.name.value, (alias.name.span, alias.module.clone()));
        }
    }
    
    let graph = make_graph(items.types.iter());
    let mut had_error = false;
    for cycle in graph.find_all_cycles() {
        let message = if cycle.len() == 2 {
            format!("Type alias '{}' depends on itself.", items.symbol_names[&cycle[0]])
        } else {
            let main = &items.symbol_names[&cycle[0]];
            let mut msg = format!("Type alias '{}' depends on itself indirectly. Dependency chain is '{}'",
                main,
                main);
            for typ in &cycle[1..] {
                msg.push_str(" -> '");
                msg.push_str(&items.symbol_names[&typ]);
                msg.push_str("'");
            }
            msg.push_str(".");
            msg
        };
        let &(span, ref module) = &positions[cycle[0]];
        errors
            .alias_expansion_error(module)
            .note(message, span)
            .done();
        had_error = true
    }

    if had_error {
        Err(())
    } else {
        Ok(())
    }
}

struct Replacement {
    vars: Vec<Sym>,
    type_: Type,
}

type Replacements = HashMap<Sym, Replacement>;

fn replace_vars(type_: &Type, vars: &HashMap<Sym, &Type>) -> Type {
    match *type_ {
        Type::Any => Type::Any,
        Type::Apply(ref a, ref b) => {
            let a = Node::new(replace_vars(&a.value, vars), DUMMY_SPAN);
            let b = Node::new(replace_vars(&b.value, vars), DUMMY_SPAN);
            Type::Apply(Box::new(a), Box::new(b))
        }
        Type::Concrete(ref name) => {
            Type::Concrete(name.clone())
        }
        Type::Function(ref a, ref b) => {
            let a = Node::new(replace_vars(&a.value, vars), DUMMY_SPAN);
            let b = Node::new(replace_vars(&b.value, vars), DUMMY_SPAN);
            Type::Function(Box::new(a), Box::new(b))
        }
        Type::SelfType => Type::SelfType,
        Type::Tuple(ref items) => {
            let mut new_items = Vec::new();
            for item in items {
                let item = replace_vars(&item.value, vars);
                new_items.push(Node::new(item, DUMMY_SPAN));
            }
            Type::Tuple(new_items)
        }
        Type::Var(var) => {
            vars.get(&var).cloned().cloned().unwrap_or(Type::Var(var))
        }
    }
}

fn make_result(
                module: &Name,
                span: Span,
                name: Sym,
                name_str: &str,
                args: &[Node<Type>],
                replacements: &Replacements,
                errors: &mut Errors) -> Type {
    if let Some(replacement) = replacements.get(&name) {
        if args.len() != replacement.vars.len() {
            let msg = format!("Type alias {} expected {} arguments, got {}.",
                name_str,
                replacement.vars.len(),
                args.len());
            errors
                .alias_expansion_error(module)
                .note(msg, span)
                .done();
            Type::Any
        } else {
            let mut vars = HashMap::new();
            for i in 0..args.len() {
                vars.insert(replacement.vars[i], &args[i].value);
            }
            replace_vars(&replacement.type_, &vars)
        }
    } else {
        let mut result = Node::new(Type::Concrete(Symbol::Known(name)), span);
        for arg in args {
            let arg = Node::new(arg.value.clone(), span);
            result = Node::new(Type::Apply(Box::new(result), Box::new(arg)), span);
        }
        result.value
    }
}

fn expand_in_type(
                    type_: &Node<Type>,
                    replacements: &Replacements,
                    symbol_names: &HashMap<Sym, String>,
                    module: &Name,
                    errors: &mut Errors) -> Node<Type> {
    let replaced = match type_.value {
        Type::Any => Type::Any,
        Type::SelfType => Type::SelfType,
        Type::Var(ref var) => Type::Var(var.clone()),
        Type::Function(ref a, ref b) => {
            let a = expand_in_type(&**a, replacements, symbol_names, module, errors);
            let b = expand_in_type(&**b, replacements, symbol_names, module, errors);
            Type::Function(Box::new(a), Box::new(b))
        }
        Type::Tuple(ref items) => {
            let mut new_items = Vec::new();
            for item in items {
                new_items.push(expand_in_type(item, replacements, symbol_names, module, errors));
            }
            Type::Tuple(new_items)
        }
        Type::Apply(ref a, ref b) => {
            let mut args = Vec::new();
            args.push(expand_in_type(&**b, replacements, symbol_names, module, errors));
            let mut matched = a;
            while let Type::Apply(ref l, ref r) = matched.value {
                args.push(expand_in_type(&**r, replacements, symbol_names, module, errors));
                matched = l;
            }
            args.reverse();
            match matched.value {
                Type::Concrete(name) => {
                    match name {
                        Symbol::Known(sym) => {
                            make_result(
                                module,
                                matched.span,
                                sym,
                                &symbol_names[&sym],
                                &args,
                                replacements,
                                errors)
                        }
                        Symbol::Unknown => {
                            Type::Concrete(Symbol::Unknown)
                        }
                    }
                }
                _ => {
                    let mut result = expand_in_type(matched, replacements, symbol_names, module, errors);
                    for item in args {
                        let type_ = Type::Apply(Box::new(result), Box::new(item));
                        result = Node::new(type_, DUMMY_SPAN);
                    }
                    result.value
                }
            }
        }
        Type::Concrete(name) => {
            match name {
                Symbol::Known(sym) => {
                    make_result(
                        module,
                        type_.span,
                        sym,
                        &symbol_names[&sym],
                        &[],
                        replacements,
                        errors)
                }
                Symbol::Unknown => {
                    Type::Concrete(Symbol::Unknown)
                }
            }
        }
    };
    Node::new(replaced, type_.span)
}

fn expand_in_annotation(
                        annot: &mut TypeAnnot,
                        replacements: &Replacements,
                        symbol_names: &HashMap<Sym, String>,
                        errors: &mut Errors) {
    let expanded = expand_in_type(
        &annot.type_.value.type_,
        replacements,
        symbol_names,
        &annot.module,
        errors);
    annot.type_.value.type_ = expanded;
}

pub fn expand_aliases(mut items: Items, errors: &mut Errors) -> Items {
    let mut replacements = HashMap::new();
    for type_ in &items.types {
        if let TypeDecl::TypeAlias(ref alias) = *type_ {
            let replacement = Replacement {
                vars: alias.vars.iter().map(|v| v.value.clone()).collect(),
                type_: alias.type_.clone().map(|t| t.value).unwrap_or(Type::Any),
            };
            replacements.insert(alias.name.value.clone(), replacement);
        }
    }
    for type_ in &mut items.types {
        match *type_ {
            TypeDecl::Record(ref mut record) => {
                for field in &mut record.fields {
                    let expanded = expand_in_type(
                        &field.1,
                        &replacements,
                        &items.symbol_names,
                        &record.module,
                        errors);
                    field.1 = expanded;
                }
            }
            TypeDecl::Union(ref mut union) => {
                for case in &mut union.cases {
                    for arg in &mut case.value.args {
                        let expanded = expand_in_type(
                            arg,
                            &replacements,
                            &items.symbol_names,
                            &union.module,
                            errors);
                        *arg = expanded;
                    }
                }
            }
            TypeDecl::TypeAlias(_) => { }
        }
    }
    for annot in items.annotations.values_mut() {
        expand_in_annotation(
            &mut annot.value,
            &replacements,
            &items.symbol_names,
            errors);
    }
    for trait_ in &mut items.traits {
        for item in &mut trait_.values {
            expand_in_annotation(
                &mut item.value,
                &replacements,
                &items.symbol_names,
                errors);
        }
    }
    items
}
