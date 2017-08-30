use std::collections::BTreeMap;
use ast::{Node, NodeView};
use ast::resolved::{Def, Expr, Sym, Symbol, Items, GroupedItems, Impl, GroupedImpl};
use compiler::util::Graph;


fn collect_deps<'a>(expr: &'a Expr, deps: &mut Vec<&'a Sym>) {
    match *expr {
        Expr::Apply(ref a, ref b) |
        Expr::DoIf(ref a, ref b) |
        Expr::Bind(_, ref a, ref b) => {
            collect_deps(&a.value, deps);
            collect_deps(&b.value, deps);
        }
        Expr::Ident(Symbol::Known(ref sym)) => {
            deps.push(sym);
        }
        Expr::Ident(Symbol::Unknown) |
        Expr::Literal(_) => { }
        Expr::If(ref a, ref b, ref c) => {
            collect_deps(&a.value, deps);
            collect_deps(&b.value, deps);
            collect_deps(&c.value, deps);
        }
        Expr::Infix(ref a, ref op, ref b) => {
            collect_deps(&a.value, deps);
            collect_deps(&b.value, deps);
            if let Symbol::Known(ref sym) = op.value {
                deps.push(sym);
            }
        }
        Expr::Parenthesised(ref e) |
        Expr::Lambda(_, ref e) => {
            collect_deps(&e.value, deps);
        }
        Expr::Let(ref defs, ref val) => {
            for def in defs {
                collect_deps(&def.value.value.value, deps);
            }
            collect_deps(&val.value, deps);
        }
        Expr::Tuple(ref items) |
        Expr::List(ref items) => {
            for item in items {
                collect_deps(&item.value, deps);
            }
        }
        Expr::Case(ref e, ref branches) => {
            collect_deps(&e.value, deps);
            for branch in branches {
                collect_deps(&branch.value.value.value, deps);
                if let Some(ref guard) = branch.value.guard {
                    collect_deps(&guard.value, deps);
                }
            }
        }
    }
}

fn make_graph<'a, D: 'a + NodeView<Def>, I: Iterator<Item=&'a D>>(defs: I) -> Graph<'a, Sym> {
    let nodes = defs.map(|def| {
        let mut deps = Vec::new();
        collect_deps(&def.inner().value.value, &mut deps);
        (&def.inner().sym.value, deps)
    });
    Graph::new(nodes)
}

fn group_defs<T: NodeView<Def>>(defs: Vec<T>) -> Vec<Vec<T>> {
    let sccs = make_graph(defs.iter())
        .to_strongly_connected_components()
        .into_iter()
        .map(|symbols| symbols.into_iter().cloned().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    let mut by_name = defs
        .into_iter()
        .map(|def| (def.inner().sym.value, def))
        .collect::<BTreeMap<_, _>>();
    let mut grouped = Vec::new();
    for scc in sccs {
        let mut defs = Vec::new();
        for vertex in scc {
            defs.push(by_name.remove(&vertex).unwrap());
        }
        grouped.push(defs);
    }
    grouped
}

fn group_let_defs(defs: Vec<Node<Def>>) -> Vec<Vec<Node<Def>>> {
    group_defs(defs)
}

fn group_top_level_defs(defs: Vec<Def>) -> Vec<Vec<Def>> {
    group_defs(defs)
}

fn group_in_expr(expr: &mut Node<Expr>) {
    let res = match expr.value {
        Expr::Apply(ref mut a, ref mut b) |
        Expr::DoIf(ref mut a, ref mut b) |
        Expr::Bind(_, ref mut a, ref mut b) |
        Expr::Infix(ref mut a, _, ref mut b) => {
            group_in_expr(a);
            group_in_expr(b);
            return;
        }
        Expr::Ident(_) |
        Expr::Literal(_) => {
            return;
        }
        Expr::If(ref mut a, ref mut b, ref mut c) => {
            group_in_expr(a);
            group_in_expr(b);
            group_in_expr(c);
            return;
        }
        Expr::Parenthesised(ref mut e) |
        Expr::Lambda(_, ref mut e) => {
            group_in_expr(e);
            return;
        }
        Expr::Let(ref mut defs, ref mut val) => {
            group_in_expr(val);
            let mut defs_vec = Vec::new();
            let mut result = Node::new(Expr::Tuple(Vec::new()), ::position::DUMMY_SPAN);
            ::std::mem::swap(&mut defs_vec, defs);
            ::std::mem::swap(&mut result, val);
            let defs_vec = group_let_defs(defs_vec);
            for group in defs_vec.into_iter().rev() {
                debug_assert!(group.len() > 0);
                let e = Expr::Let(group, Box::new(result));
                result = Node::new(e, expr.span);
            }
            result
        }
        Expr::Tuple(ref mut items) |
        Expr::List(ref mut items) => {
            for item in items {
                group_in_expr(item);
            }
            return;
        }
        Expr::Case(ref mut e, ref mut branches) => {
            group_in_expr(e);
            for branch in branches {
                group_in_expr(&mut branch.value.value);
                if let Some(ref mut guard) = branch.value.guard {
                    group_in_expr(guard);
                }
            }
            return;
        }
    };
    *expr = res;
}

fn group_impl_items(impl_: Impl) -> GroupedImpl {
    let Impl { scheme, trait_, mut values, trait_items, module } = impl_;
    for value in &mut values {
        group_in_expr(&mut value.value.value);
    }
    let values = group_let_defs(values);
    GroupedImpl { scheme, trait_, values, trait_items, module }
}

pub fn group_items(items: Items) -> GroupedItems {
    let Items {
        types,
        mut items,
        traits,
        impls,
        annotations,
        fixities,
        symbol_names
    } = items;
    for item in &mut items {
        group_in_expr(&mut item.value);
    }
    let items = group_top_level_defs(items);
    let impls = impls.into_iter().map(group_impl_items).collect();
    GroupedItems {
        types,
        items,
        traits,
        impls,
        annotations,
        fixities,
        symbol_names,
    }
}
