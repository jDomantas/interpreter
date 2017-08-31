use std::collections::{BTreeSet, BTreeMap};
use ast::{Node, Sym, Symbol};
use ast::resolved::Type;


pub struct Graph<'a, T: 'a + ?Sized> {
    nodes: BTreeMap<&'a T, Vec<&'a T>>,
}

impl<'a, T: Ord + ?Sized> Graph<'a, T> {
    pub fn new<I>(nodes: I) -> Self where I: Iterator<Item=(&'a T, Vec<&'a T>)> {
        Graph {
            nodes: nodes.collect()
        }
    }

    fn find_cycle(&self) -> Option<Vec<&'a T>> {
        let mut stack = Vec::new();
        let mut in_stack = BTreeSet::new();
        let mut visited = BTreeSet::new();
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
                current: &'a T,
                stack: &mut Vec<&'a T>,
                in_stack: &mut BTreeSet<&'a T>,
                visited: &mut BTreeSet<&'a T>) -> Option<Vec<&'a T>> {
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
        if let Some(neighbour_list) = self.nodes.get(current) {
            stack.push(current);
            in_stack.insert(current);
            visited.insert(current);
            for ng in neighbour_list {
                if let Some(cycle) = self.dfs_walk(ng, stack, in_stack, visited) {
                    return Some(cycle);
                }
            }
            in_stack.remove(current);
            stack.pop();
        }
        None
    }

    fn remove_node(&mut self, name: &T) {
        self.nodes.remove(name);
    }

    pub fn find_all_cycles(mut self) -> Vec<Vec<&'a T>> {
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

    pub fn to_strongly_connected_components(self) -> Vec<Vec<&'a T>> {
        let ctx = TarjanCtx::new(&self.nodes);
        ctx.find_components()
    }
}

struct TarjanCtx<'a: 'b, 'b, T: 'a + ?Sized> {
    neighbours: &'b BTreeMap<&'a T, Vec<&'a T>>,
    stack: Vec<&'a T>,
    on_stack: BTreeSet<&'a T>,
    next_index: usize,
    index: BTreeMap<&'a T, usize>,
    components: Vec<Vec<&'a T>>,
    lowlink: BTreeMap<&'a T, usize>,
}

impl<'a, 'b, T: Ord + ?Sized> TarjanCtx<'a, 'b, T> {
    fn new(neighbours: &'b BTreeMap<&'a T, Vec<&'a T>>) -> Self {
        TarjanCtx {
            neighbours: neighbours,
            stack: Vec::new(),
            on_stack: BTreeSet::new(),
            next_index: 0,
            index: BTreeMap::new(),
            components: Vec::new(),
            lowlink: BTreeMap::new(),
        }
    }

    fn find_components(mut self) -> Vec<Vec<&'a T>> {
        for &v in self.neighbours.keys() {
            if !self.index.contains_key(v) {
                self.connect(v);
            }
        }

        self.components
    }

    fn connect(&mut self, node: &'a T) -> usize {
        self.index.insert(node, self.next_index);
        self.lowlink.insert(node, self.next_index);
        self.next_index += 1;
        self.on_stack.insert(node);
        self.stack.push(node);

        for &ng in &self.neighbours[node] {
            if !self.neighbours.contains_key(&ng) {
                continue;
            }
            if !self.index.contains_key(ng) {
                let link = self.connect(ng);
                let my_link = ::std::cmp::min(self.lowlink[node], link);
                self.lowlink.insert(node, my_link);
            } else if self.on_stack.contains(ng) {
                let index = self.index[ng];
                let my_link = ::std::cmp::min(self.lowlink[node], index);
                self.lowlink.insert(node, my_link);
            }
        }

        if self.index[node] == self.lowlink[node] {
            let mut scc = Vec::new();
            loop {
                let n = self.stack.pop().unwrap();
                self.on_stack.remove(n);
                scc.push(n);
                if node == n {
                    break;
                }
            }

            self.components.push(scc);
        }

        self.lowlink[node]
    }
}

pub fn collect_concrete_types<'a>(type_: &'a Node<Type>, result: &mut Vec<&'a Sym>) {
    match type_.value {
        Type::Any | Type::SelfType | Type::Var(_) => { }
        Type::Concrete(ref name) => {
            match *name {
                Symbol::Known(ref sym) => result.push(sym),
                Symbol::Unknown => { }
            }
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

pub fn slice_from_ref<T>(item: &T) -> &[T] {
    unsafe { ::std::slice::from_raw_parts(item, 1) }
}
