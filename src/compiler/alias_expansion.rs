use std::collections::HashMap;
use ast::Node;
use ast::resolved::{Items, TypeDecl, Type, Expr, TypeAnnot, DoExpr};
use compiler::util::{self, Graph};
use errors::{self, Error};
use position::{Span, DUMMY_SPAN};


fn make_graph<'a, I: Iterator<Item=&'a TypeDecl>>(decls: I) -> Graph<'a, str> {
    let nodes = decls.filter_map(|decl| {
        if let TypeDecl::TypeAlias(ref alias) = *decl {
            let mut depends_on = Vec::new();
            let name = alias.name.value.as_ref();
            if let Some(ref type_) = alias.type_ {
                util::collect_concrete_types(type_, &mut depends_on);
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
        err.push(errors::type_alias_error(message, span, module));
    }

    err
}

struct Replacement {
    vars: Vec<String>,
    type_: Type,
}

type Replacements = HashMap<String, Replacement>;

fn replace_vars(type_: &Type, vars: &HashMap<&str, &Type>) -> Type {
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
        Type::Var(ref var) => {
            vars.get(var as &str).cloned().cloned().unwrap_or_else(|| Type::Var(var.clone()))
        }
    }
}

fn make_result(
                module: &str,
                span: Span,
                name: &str,
                args: &[Node<Type>],
                replacements: &Replacements,
                errors: &mut Vec<Error>) -> Type {
    if let Some(replacement) = replacements.get(name) {
        if args.len() != replacement.vars.len() {
            let msg = format!("Type alias {} expected {} arguments, got {}.",
                name,
                replacement.vars.len(),
                args.len());
            errors.push(errors::type_alias_error(msg, span, module));
            Type::Any
        } else {
            let mut vars = HashMap::new();
            for i in 0..args.len() {
                vars.insert(&replacement.vars[i] as &str, &args[i].value);
            }
            replace_vars(&replacement.type_, &vars)
        }
    } else {
        let mut result = Node::new(Type::Concrete(name.into()), DUMMY_SPAN);
        for arg in args {
            let arg = Node::new(arg.value.clone(), DUMMY_SPAN);
            result = Node::new(Type::Apply(Box::new(result), Box::new(arg)), DUMMY_SPAN);
        }
        result.value
    }
}

fn expand_in_type(
                    type_: &Node<Type>,
                    replacements: &Replacements,
                    module: &str,
                    errors: &mut Vec<Error>) -> Node<Type> {
    let replaced = match type_.value {
        Type::Any => Type::Any,
        Type::SelfType => Type::SelfType,
        Type::Var(ref var) => Type::Var(var.clone()),
        Type::Function(ref a, ref b) => {
            let a = expand_in_type(&**a, replacements, module, errors);
            let b = expand_in_type(&**b, replacements, module, errors);
            Type::Function(Box::new(a), Box::new(b))
        }
        Type::Tuple(ref items) => {
            let mut new_items = Vec::new();
            for item in items {
                new_items.push(expand_in_type(item, replacements, module, errors));
            }
            Type::Tuple(new_items)
        }
        Type::Apply(ref a, ref b) => {
            let mut args = Vec::new();
            args.push(expand_in_type(&**b, replacements, module, errors));
            let mut matched = a;
            while let Type::Apply(ref l, ref r) = matched.value {
                args.push(expand_in_type(&**r, replacements, module, errors));
                matched = l;
            }
            args.reverse();
            match matched.value {
                Type::Concrete(ref name) => {
                    make_result(module, matched.span, name, &args, replacements, errors)
                }
                _ => {
                    let mut result = expand_in_type(matched, replacements, module, errors);
                    for item in args {
                        let type_ = Type::Apply(Box::new(result), Box::new(item));
                        result = Node::new(type_, DUMMY_SPAN);
                    }
                    result.value
                }
            }
        }
        Type::Concrete(ref name) => {
            make_result(module, type_.span, name, &[], replacements, errors)
        }
    };
    Node::new(replaced, type_.span)
}

fn expand_in_expr(
                    expr: &mut Node<Expr>,
                    replacements: &Replacements,
                    module: &str,
                    errors: &mut Vec<Error>) {
    match expr.value {
        Expr::Apply(ref mut a, ref mut b) => {
            expand_in_expr(&mut **a, replacements, module, errors);
            for val in b {
                expand_in_expr(val, replacements, module, errors);
            }
        }
        Expr::Case(ref mut value, ref mut branches) => {
            expand_in_expr(&mut **value, replacements, module, errors);
            for branch in branches {
                if let Some(ref mut guard) = branch.value.guard {
                    expand_in_expr(guard, replacements, module, errors);
                }
                expand_in_expr(&mut branch.value.value, replacements, module, errors);
            }
        }
        Expr::Do(ref mut do_) => {
            expand_in_do_expr(&mut **do_, replacements, module, errors);
        }
        Expr::Ident(_) |
        Expr::Literal(_) => { }
        Expr::If(ref mut cond, ref mut then, ref mut else_) => {
            expand_in_expr(&mut **cond, replacements, module, errors);
            expand_in_expr(&mut **then, replacements, module, errors);
            expand_in_expr(&mut **else_, replacements, module, errors);
        }
        Expr::Infix(ref mut lhs, _, ref mut rhs) => {
            expand_in_expr(&mut **lhs, replacements, module, errors);
            expand_in_expr(&mut **rhs, replacements, module, errors);
        }
        Expr::Lambda(_, ref mut value) |
        Expr::Parenthesised(ref mut value) => {
            expand_in_expr(&mut **value, replacements, module, errors);
        }
        Expr::Let(ref mut defs, ref mut annots, ref mut value) => {
            for def in defs {
                if let Some(ref mut value) = def.value.value {
                    expand_in_expr(value, &replacements, module, errors);
                }
            }
            for annot in annots {
                expand_in_annotation(&mut annot.value, &replacements, Some(module), errors);
            }
            expand_in_expr(&mut **value, replacements, module, errors);
        }
        Expr::List(ref mut items) | Expr::Tuple(ref mut items) => {
            for item in items {
                expand_in_expr(item, replacements, module, errors);
            }
        }
    }
}

fn expand_in_do_expr(
                        expr: &mut Node<DoExpr>,
                        replacements: &Replacements,
                        module: &str,
                        errors: &mut Vec<Error>) {
    match expr.value {
        DoExpr::Bind(_, ref mut value, ref mut rest) |
        DoExpr::If(ref mut value, ref mut rest) |
        DoExpr::Let(_, ref mut value, ref mut rest) |
        DoExpr::Sequence(ref mut value, ref mut rest) => {
            expand_in_expr(value, replacements, module, errors);
            expand_in_do_expr(&mut **rest, replacements, module, errors);
        }
        DoExpr::Done(ref mut value) => {
            expand_in_expr(value, replacements, module, errors);
        }
    }
}

fn expand_in_annotation(
                        annot: &mut TypeAnnot,
                        replacements: &Replacements,
                        module: Option<&str>,
                        errors: &mut Vec<Error>) {
    if let Some(ref mut type_) = annot.type_ {
        let module = module.unwrap_or(&annot.module);
        let expanded = expand_in_type(&type_.value.type_, replacements, module, errors);
        type_.value.type_ = expanded;
    }
}

pub fn expand_aliases(mut items: Items) -> (Items, Vec<Error>) {
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
    let mut errors = Vec::new();
    for type_ in &mut items.types {
        match *type_ {
            TypeDecl::Record(ref mut record) => {
                for field in &mut record.fields {
                    let expanded = expand_in_type(
                        &field.1,
                        &replacements,
                        &record.module,
                        &mut errors);
                    field.1 = expanded;
                }
            }
            TypeDecl::Union(ref mut union) => {
                for case in &mut union.cases {
                    for arg in &mut case.value.args {
                        let expanded = expand_in_type(
                            arg,
                            &replacements,
                            &union.module,
                            &mut errors);
                        *arg = expanded;
                    }
                }
            }
            TypeDecl::TypeAlias(_) => { }
        }
    }
    for def in &mut items.items {
        if let Some(ref mut value) = def.value {
            expand_in_expr(value, &replacements, &def.module, &mut errors);
        }
    }
    for impl_ in &mut items.impls {
        for def in &mut impl_.values {
            if let Some(ref mut value) = def.value.value {
                expand_in_expr(value, &replacements, &impl_.module, &mut errors);
            }
        }
    }
    for annot in items.annotations.values_mut() {
        expand_in_annotation(annot, &replacements, None, &mut errors);
    }
    for trait_ in &mut items.traits {
        for item in &mut trait_.values {
            expand_in_annotation(&mut item.value, &replacements, Some(&trait_.module), &mut errors);
        }
    }
    (items, errors)
}
