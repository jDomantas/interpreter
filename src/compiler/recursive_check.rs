use std::collections::{HashSet, HashMap};
use ast::Node;
use ast::resolved::{Items, TypeDecl, TypeAlias, Type};
use errors::{self, Error};
use position::Span;

struct GraphNode<'a> {
    name: &'a str,
    parents: Vec<&'a str>,
}

struct Graph<'a> {
    nodes: HashMap<&'a str, GraphNode<'a>>,
}

impl<'a> Graph<'a> {
    fn from_aliases<I: Iterator<Item=&'a TypeAlias>>(aliases: I) -> Graph<'a> {
        let mut nodes = HashMap::new();
        for alias in aliases {
            let name = alias.name.value.as_ref();
            let mut parents = Vec::new();
            if let Some(ref type_) = alias.type_ {
                collect_concrete_types(type_, &mut parents);
            }
            let node = GraphNode {
                name: name,
                parents: parents,
            };
            nodes.insert(name, node);
        }
        Graph {
            nodes: nodes,
        }
    }

    fn find_cycle(&self) -> Option<Vec<&'a str>> {
        let mut stack = Vec::new();
        let mut in_stack = HashSet::new();
        let mut visited = HashSet::new();
        for &node in self.nodes.keys() {
            let cycle = self.dfs_walk(node, &mut stack, &mut in_stack, &mut visited);
            if cycle.is_some() {
                return cycle;
            }
        }
        None
    }

    fn dfs_walk(
                &self,
                current: &'a str,
                stack: &mut Vec<&'a str>,
                in_stack: &mut HashSet<&'a str>,
                visited: &mut HashSet<&'a str>) -> Option<Vec<&'a str>> {
        if in_stack.contains(current) {
            let mut cycle = vec![current];
            loop {
                let item = stack.pop().unwrap();
                cycle.push(item);
                if item == current {
                    cycle.reverse();
                    return Some(cycle);
                }
            }
        }
        if visited.contains(current) {
            return None;
        }
        if let Some(ref node) = self.nodes.get(current) {
            stack.push(current);
            in_stack.insert(current);
            visited.insert(current);
            for &ng in &node.parents {
                match self.dfs_walk(ng, stack, in_stack, visited) {
                    Some(cycle) => return Some(cycle),
                    None => { }
                }
            }
            in_stack.remove(current);
            stack.pop();
        }
        None
    }

    fn remove_node(&mut self, name: &str) {
        self.nodes.remove(name);
    }

    fn find_all_cycles(mut self) -> Vec<Vec<&'a str>> {
        let mut cycles = Vec::new();
        while let Some(cycle) = self.find_cycle() {
            debug_assert!(cycle.len() >= 2);
            for &node in &cycle {
                self.remove_node(node);
            }
            cycles.push(cycle);
        }
        cycles
    }
}

fn collect_concrete_types<'a>(type_: &'a Node<Type>, result: &mut Vec<&'a str>) {
    match type_.value {
        Type::SelfType => { }
        Type::Var(_) => { }
        Type::Concrete(ref name) => {
            result.push(name);
        }
        Type::Apply(ref a, ref b) => {
            collect_concrete_types(a, result);
            collect_concrete_types(b, result);
        }
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

pub fn find_alias_cycles(items: &Items) -> Vec<Error> {
    let mut positions = HashMap::<&str, Span>::new();
    let mut err = Vec::new();
    let graph = {
        let aliases = items.types.iter().filter_map(|item| {
            match *item {
                TypeDecl::TypeAlias(ref alias) => {
                    positions.insert(alias.name.value.as_ref(), alias.name.span);
                    Some(alias)
                }
                _ => None,
            }
        });

        Graph::from_aliases(aliases)
    };
    
    for cycle in graph.find_all_cycles() {
        let message = if cycle.len() == 2 {
            format!("Type alias '{}' depends on itself.", cycle[0])
        } else {
            let main = cycle[0];
            let mut msg = format!("Type alias '{}' depends on itself. Dependency chain is '{}'",
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
        let span = *positions.get(cycle[0]).unwrap();
        let module = errors::symbol_module(cycle[0]);
        err.push(errors::recursive_type_alias(message, span, module));
    }

    err
}
