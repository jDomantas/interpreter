use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry;
use ast::{
    Module, RawSymbol, Symbol, Decl, LetDecl, Node, ItemList, Impl, Def,
    TypeAnnot, RecordType, Trait, TypeAlias, UnionType, Type, Expr, Pattern,
    UnionCase, Scheme, TraitBound,
};
use errors::{self, Error};
use position::Span;


#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Hash, Clone)]
struct Item<'a> {
    name: &'a str,
    parent: Option<&'a str>,
}

impl<'a> Item<'a> {
    fn new(name: &'a str, parent: Option<&'a str>) -> Item<'a> {
        Item {
            name: name,
            parent: parent,
        }
    }
}

#[derive(PartialEq, Eq, Clone)]
struct Exports<'a> {
    types: HashSet<&'a str>,
    traits: HashSet<&'a str>,
    patterns: HashSet<Item<'a>>,
    values: HashSet<Item<'a>>,
}

struct Context<'a> {
    module: &'a str,
    imports: &'a Imports<'a>,
    exports: &'a HashMap<String, Exports<'a>>,
}

impl<'a> Exports<'a> {
    fn empty() -> Exports<'a> {
        Exports {
            types: HashSet::new(),
            traits: HashSet::new(),
            patterns: HashSet::new(),
            values: HashSet::new(),
        }
    }

    fn has_type(&self, type_: &str) -> bool {
        self.types.contains(type_)
    }

    fn has_trait(&self, trait_: &str) -> bool {
        self.traits.contains(trait_)
    }

    fn has_pattern(&self, pattern: &str) -> bool {
        for pat in &self.patterns {
            if pat.name == pattern {
                return true;
            }
        }
        false
    }

    fn has_value(&self, value: &str) -> bool {
        for val in &self.values {
            if val.name == value {
                return true;
            }
        }
        false
    }
}

struct Imports<'a> {
    modules: HashMap<&'a str, &'a str>,
    types: HashMap<&'a str, &'a str>,
    traits: HashMap<&'a str, &'a str>,
    patterns: HashMap<&'a str, &'a str>,
    values: HashMap<&'a str, &'a str>,
}

impl<'a> Imports<'a> {
    fn empty() -> Imports<'a> {
        Imports {
            modules: HashMap::new(),
            types: HashMap::new(),
            traits: HashMap::new(),
            patterns: HashMap::new(),
            values: HashMap::new(),
        }
    }

    fn add_locals(&mut self, my_name: &'a str, locals: &Exports<'a>) {
        for type_ in &locals.types {
            self.types.insert(*type_, my_name);
        }
        for trait_ in &locals.traits {
            self.traits.insert(*trait_, my_name);
        }
        for pattern in &locals.patterns {
            self.patterns.insert(pattern.name, my_name);
        }
        for value in &locals.values {
            self.values.insert(value.name, my_name);
        }
    }
}

type ModuleTable<Sym> = HashMap<String, Module<Sym>>;

pub fn resolve_symbols(modules: ModuleTable<RawSymbol>) -> Result<ModuleTable<Symbol>, Vec<Error>> {
    unimplemented!()
}

struct Resolver {
    errors: Vec<Error>,
    emit_errors: bool,
}

impl Resolver {
    fn new() -> Resolver {
        Resolver {
            errors: Vec::new(),
            emit_errors: true,
        }
    }

    fn double_definition(&mut self, name: &str, span: Span, previous: Span, module: &str) {
        if self.emit_errors {
            let message = format!("Item `{}` is defined multiple times.", name);
            let previous_message = "Note: previously defined here.";
            self.errors.push(errors::double_symbol_error(
                message,
                span,
                previous_message,
                previous,
                module));
        }
    }

    fn bad_export(&mut self, message: String, span: Span, module: &str) {
        if self.emit_errors {
            self.errors.push(errors::symbol_error(message, span, module));
        }
    }

    fn module_double_import(&mut self, offending: &str, span: Span, previous: Span, module: &str) {
        if self.emit_errors {
            let message = format!("Module `{}` is imported twice.", offending);
            let previous_message = "Note: previously imported here.";
            self.errors.push(errors::double_symbol_error(
                message,
                span,
                previous_message,
                previous,
                module));
        }
    }

    fn double_import(&mut self, item: &str, span: Span, previous: Span, module: &str) {
        if self.emit_errors {
            let message = format!("Item `{}` is imported multiple times.", item);
            let previous_message = "Note: previously imported here.";
            self.errors.push(errors::double_symbol_error(
                message,
                span,
                previous_message,
                previous,
                module));
        }
    }

    fn not_exported(&mut self, item: &str, by: &str, span: Span, module: &str) {
        if self.emit_errors {
            let message = format!("Item `{}` is not exported by `{}`.", item, by);
            self.errors.push(errors::symbol_error(
                message,
                span,
                module));
        }
    }

    fn subitem_not_exported(&mut self, item: &str, parent: &str, by: &str, span: Span, module: &str) {
        if self.emit_errors {
            let message = format!("Module `{}` does not export `{}` as subitem of `{}`.", by, item, parent);
            self.errors.push(errors::symbol_error(
                message,
                span,
                module));
        }
    }

    fn no_subitems(&mut self, item: &str, by: &str, span: Span, module: &str) {
        if self.emit_errors {
            let message = format!("Module `{}` does not export any subitems of `{}`.", by, item);
            self.errors.push(errors::symbol_error(
                message,
                span,
                module));
        }
    }

    fn unknown_symbol(&mut self, kind: &str, symbol: &Node<RawSymbol>, module: &str) {
        if self.emit_errors {
            let message = format!("Unknown {}: `{}`.", kind, symbol.node.clone().full_name());
            self.errors.push(errors::symbol_error(message, symbol.span, module));
        }
    }

    fn unknown_module(&mut self, m: &str, span: Span, module: &str) {
        if self.emit_errors {
            let message = format!("Unknown module: `{}`.", m);
            self.errors.push(errors::symbol_error(message, span, module));
        }
    }

    fn collect_items<'a>(&mut self, module: &'a Module<RawSymbol>) -> Exports<'a> {
        let mut values: HashMap<&'a str, (Option<&'a str>, Span)> = HashMap::new();
        let mut patterns: HashMap<&'a str, (Option<&'a str>, Span)> = HashMap::new();
        let mut traits: HashMap<&'a str, Span> = HashMap::new();
        let mut types: HashMap<&'a str, Span> = HashMap::new();

        for item in &module.items {
            match item.node {
                Decl::Let(LetDecl::Def(ref def)) => {
                    let vars = def.pattern.node.bound_vars(&def.pattern.span);
                    for Node { node: var, span, .. } in vars {
                        match values.entry(var) {
                            Entry::Vacant(entry) => {
                                entry.insert((None, span));
                            }
                            Entry::Occupied(entry) => {
                                let another = entry.get().1;
                                // duplicate value, defined at `span` and `another`
                                self.double_definition(
                                    var,
                                    span,
                                    another,
                                    module.name());
                            }
                        }
                    }
                }
                Decl::Record(ref record) => {
                    if let Some(span) = traits.get(record.name.node.as_str()) {
                        // duplicate type, defined at `record.name.span` and `span`
                        self.double_definition(
                            &record.name.node,
                            record.name.span,
                            *span,
                            module.name());
                    } else {
                        match types.entry(&record.name.node) {
                            Entry::Vacant(entry) => {
                                entry.insert(record.name.span);
                            }
                            Entry::Occupied(entry) => {
                                let span = entry.get();
                                // duplicate type, defined at `record.name.span` and `span`
                                self.double_definition(
                                    &record.name.node,
                                    record.name.span,
                                    *span,
                                    module.name());
                            }
                        }
                    }
                    match values.entry(&record.name.node) {
                        Entry::Vacant(entry) => {
                            entry.insert((None, record.name.span));
                        }
                        Entry::Occupied(entry) => {
                            let span = entry.get().1;
                            // duplicate value, defined at `record.name.span` and `span`
                            self.double_definition(
                                &record.name.node,
                                record.name.span,
                                span,
                                module.name());
                        }
                    }
                    match patterns.entry(&record.name.node) {
                        Entry::Vacant(entry) => {
                            entry.insert((None, record.name.span));
                        }
                        Entry::Occupied(entry) => {
                            let span = entry.get().1;
                            // duplicate pattern, defined at `record.name.span` and `span`
                            self.double_definition(
                                &record.name.node,
                                record.name.span,
                                span,
                                module.name());
                        }
                    }
                    for field in &record.fields {
                        match values.entry(&field.0.node) {
                            Entry::Vacant(entry) => {
                                entry.insert((Some(&record.name.node), field.0.span));
                            }
                            Entry::Occupied(entry) => {
                                let span = entry.get().1;
                                // duplicate value, defined at `field.0.span` and `span`
                                self.double_definition(
                                    &field.0.node,
                                    field.0.span,
                                    span,
                                    module.name());
                            }
                        }
                    }
                }
                Decl::Trait(ref trait_) => {
                    if let Some(span) = types.get(trait_.name.node.as_str()) {
                        // duplicate type, defined at `trait_.name.span` and `span`
                        self.double_definition(
                            &trait_.name.node,
                            trait_.name.span,
                            *span,
                            module.name());
                    } else {
                        match traits.entry(&trait_.name.node) {
                            Entry::Vacant(entry) => {
                                entry.insert(trait_.name.span);
                            }
                            Entry::Occupied(entry) => {
                                let span = entry.get();
                                // duplicate trait, defined at `trait_.name.span` and `span`
                                self.double_definition(
                                    &trait_.name.node,
                                    trait_.name.span,
                                    *span,
                                    module.name());
                            }
                        }
                    }

                    for value in &trait_.values {
                        let name = &value.node.value;
                        let item = match name.node {
                            RawSymbol::Unqualified(ref s) => {
                                s
                            }
                            _ => {
                                panic!("type annotation should contain unqualified symbol");
                            }
                        };
                        match values.entry(item) {
                            Entry::Vacant(entry) => {
                                entry.insert((None, value.node.value.span));
                            }
                            Entry::Occupied(entry) => {
                                let another = entry.get().1;
                                // duplicate value, defined at `span` and `another`
                                self.double_definition(
                                    item,
                                    value.node.value.span,
                                    another,
                                    module.name());
                            }
                        }
                    }
                }
                Decl::TypeAlias(ref type_) => {
                    if let Some(span) = traits.get(type_.name.node.as_str()) {
                        // duplicate type, defined at `type_.name.span` and `span`
                        self.double_definition(
                            &type_.name.node,
                            type_.name.span,
                            *span,
                            module.name());
                    } else {
                        match types.entry(&type_.name.node) {
                            Entry::Vacant(entry) => {
                                entry.insert(type_.name.span);
                            }
                            Entry::Occupied(entry) => {
                                let span = entry.get();
                                // duplicate type, defined at `type_.name.span` and `span`
                                self.double_definition(
                                    &type_.name.node,
                                    type_.name.span,
                                    *span,
                                    module.name());
                            }
                        }
                    }
                }
                Decl::Union(ref union) => {
                    match types.entry(&union.name.node) {
                        Entry::Vacant(entry) => {
                            entry.insert(union.name.span);
                        }
                        Entry::Occupied(entry) => {
                            let span = entry.get();
                            // duplicate type, defined at `span` and `union.name.span`
                            self.double_definition(
                                &union.name.node,
                                union.name.span,
                                *span,
                                module.name());
                        }
                    }
                    for case in &union.cases {
                        match values.entry(&case.node.tag.node) {
                            Entry::Vacant(entry) => {
                                entry.insert((Some(&union.name.node), case.node.tag.span));
                            }
                            Entry::Occupied(entry) => {
                                let span = entry.get().1;
                                // duplicate value, defined at `span` and `case.node.tag.span`
                                self.double_definition(
                                    &case.node.tag.node,
                                    case.node.tag.span,
                                    span,
                                    module.name());
                            }
                        }
                        match patterns.entry(&case.node.tag.node) {
                            Entry::Vacant(entry) => {
                                entry.insert((Some(&union.name.node), case.node.tag.span));
                            }
                            Entry::Occupied(entry) => {
                                let span = entry.get().1;
                                // duplicate pattern, defined at `span` and `case.node.tag.span`
                                self.double_definition(
                                    &case.node.tag.node,
                                    case.node.tag.span,
                                    span,
                                    module.name());
                            }
                        }
                    }
                }
                Decl::Infix(_, _, _) => { }
                Decl::Impl(_) => { }
                Decl::Let(LetDecl::Type(_)) => { }
            }
        }

        let values = values.into_iter().map(|a| Item::new(a.0, (a.1).0)).collect();
        let patterns = patterns.into_iter().map(|a| Item::new(a.0, (a.1).0)).collect();
        let types = types.into_iter().map(|a| a.0).collect();
        let traits = traits.into_iter().map(|a| a.0).collect();

        Exports {
            values: values,
            patterns: patterns,
            types: types,
            traits: traits,
        }
    }

    fn collect_exports<'a>(&mut self, module: &'a Module<RawSymbol>) -> Exports<'a> {
        let items = self.collect_items(module);

        let exposed = match module.def.node.exposing.node {
            ItemList::All => return items,
            ItemList::Some(ref items) => items,
        };

        let mut result = Exports::empty();
        for item in exposed {
            let subitems = &item.node.subitems;
            let name = &item.node.name.node;
            let span = item.node.name.span;
            let item = Item::new(name, None);
            let mut found = false;
            if items.values.contains(&item) {
                result.values.insert(item.clone());
                found = true;
            }
            if items.patterns.contains(&item) {
                result.patterns.insert(item.clone());
                found = true;
            }
            if items.types.contains(name.as_str()) {
                result.types.insert(name);
                found = true;
            }
            if items.traits.contains(name.as_str()) {
                result.traits.insert(name);
                found = true;
            }
            if !found {
                self.bad_export(
                    format!("`{}` is not defined.", name),
                    span,
                    module.name());
                continue;
            }
            match *subitems {
                Some(Node { node: ItemList::All, .. }) => {
                    let mut found_any = false;
                    for value in &items.values {
                        if value.parent == Some(name) {
                            result.values.insert(value.clone());
                            found_any = true;
                        }
                    }
                    for pattern in &items.patterns {
                        if pattern.parent == Some(name) {
                            result.patterns.insert(pattern.clone());
                            found_any = true;
                        }
                    }
                    if !found_any {
                        self.bad_export(
                            format!("`{}` does not have any subitems.", name),
                            span,
                            module.name());
                    }
                }
                Some(Node { node: ItemList::Some(ref subitems), .. }) => {
                    for item in subitems {
                        let item = Item::new(&item.node, Some(name));
                        let mut is_ok = false;
                        if items.values.contains(&item) {
                            result.values.insert(item.clone());
                            is_ok = true;
                        }
                        if items.patterns.contains(&item) {
                            result.patterns.insert(item.clone());
                            is_ok = true;
                        }
                        if !is_ok {
                            self.bad_export(
                                format!("`{}` does not have subitem `{}`.", name, item.name),
                                span,
                                module.name());
                        }
                    }
                }
                None => { }
            }
        }

        result
    }

    fn collect_imports<'a>(&mut self, module: &'a Module<RawSymbol>, ctx: &Context<'a>) -> Imports<'a> {
        let mut imports = Imports::empty();
        let mut import_span = HashMap::new();
        let mut first_import = HashMap::new();

        for import in &module.imports {
            let alias: &str = if let Some(ref alias) = import.node.alias {
                &alias.node
            } else {
                &import.node.name.node
            };

            match imports.modules.entry(alias) {
                Entry::Vacant(entry) => {
                    entry.insert(&import.node.name.node);
                    import_span.insert(alias, import.span);
                }
                Entry::Occupied(_) => {
                    let previous = *import_span.get(alias).unwrap();
                    self.module_double_import(alias, import.node.name.span, previous, module.name());
                    continue;
                }
            }

            let name = &import.node.name.node;

            let exports = if let Some(exports) = ctx.exports.get(name) {
                exports
            } else {
                panic!("all required modules should be available at symbol resolution");
            };

            match import.node.exposing {
                Some(Node { node: ItemList::All, span: list_span, .. }) => {
                    for type_ in &exports.types {
                        match first_import.entry(*type_) {
                            Entry::Vacant(entry) => {
                                entry.insert((name, list_span));
                                imports.types.insert(*type_, name);
                            }
                            Entry::Occupied(entry) => {
                                self.double_import(type_, list_span, entry.get().1, module.name());
                            }
                        }
                    }
                    for trait_ in &exports.traits {
                        match first_import.entry(*trait_) {
                            Entry::Vacant(entry) => {
                                entry.insert((name, list_span));
                                imports.traits.insert(*trait_, name);
                            }
                            Entry::Occupied(entry) => {
                                self.double_import(trait_, list_span, entry.get().1, module.name());
                            }
                        }
                    }
                    for pattern in &exports.patterns {
                        match first_import.entry(pattern.name) {
                            Entry::Vacant(entry) => {
                                entry.insert((name, list_span));
                                imports.patterns.insert(pattern.name, name);
                            }
                            Entry::Occupied(entry) => {
                                self.double_import(pattern.name, list_span, entry.get().1, module.name());
                            }
                        }
                    }
                    for value in &exports.values {
                        match first_import.entry(value.name) {
                            Entry::Vacant(entry) => {
                                entry.insert((name, list_span));
                                imports.values.insert(value.name, name);
                            }
                            Entry::Occupied(entry) => {
                                self.double_import(value.name, list_span, entry.get().1, module.name());
                            }
                        }
                    }
                }
                Some(Node { node: ItemList::Some(ref items), .. }) => {
                    for item in items {
                        let item_name = &item.node.name.node;
                        let item_span = item.node.name.span;
                        match first_import.entry(item_name) {
                            Entry::Vacant(entry) => {
                                entry.insert((name, item_span));
                            }
                            Entry::Occupied(entry) => {
                                self.double_import(item_name, item_span, entry.get().1, module.name());
                                continue;
                            }
                        }
                        let searched = Item::new(item_name, None);
                        let mut is_ok = false;
                        if exports.types.contains(item_name.as_str()) {
                            imports.types.insert(item_name, name);
                            is_ok = true;
                        }
                        if exports.traits.contains(item_name.as_str()) {
                            imports.traits.insert(item_name, name);
                            is_ok = true;
                        }
                        if exports.patterns.contains(&searched) {
                            imports.patterns.insert(item_name, name);
                            is_ok = true;
                        }
                        if exports.values.contains(&searched) {
                            imports.values.insert(item_name, name);
                            is_ok = true;
                        }
                        if !is_ok {
                            self.not_exported(
                                item_name,
                                name,
                                item_span,
                                module.name());
                            continue;
                        }
                        match item.node.subitems {
                            Some(Node { node: ItemList::All, span: list_span, .. }) => {
                                let mut found_any = false;
                                for pattern in &exports.patterns {
                                    if pattern.parent != Some(item_name) {
                                        continue;
                                    }
                                    found_any = true;
                                    match first_import.entry(pattern.name) {
                                        Entry::Vacant(entry) => {
                                            entry.insert((name, list_span));
                                            imports.patterns.insert(pattern.name, name);
                                        }
                                        Entry::Occupied(entry) => {
                                            self.double_import(
                                                pattern.name,
                                                list_span,
                                                entry.get().1,
                                                module.name());
                                        }
                                    }
                                }
                                for value in &exports.values {
                                    if value.parent != Some(item_name) {
                                        continue;
                                    }
                                    found_any = true;
                                    match first_import.entry(value.name) {
                                        Entry::Vacant(entry) => {
                                            entry.insert((name, list_span));
                                            imports.values.insert(value.name, name);
                                        }
                                        Entry::Occupied(entry) => {
                                            self.double_import(
                                                value.name,
                                                list_span,
                                                entry.get().1,
                                                module.name());
                                        }
                                    }
                                }
                                if !found_any {
                                    self.no_subitems(
                                        &item.node.name.node,
                                        name,
                                        list_span,
                                        module.name());
                                }
                            }
                            Some(Node { node: ItemList::Some(ref items), .. }) => {
                                for subitem in items {
                                    match first_import.entry(&subitem.node) {
                                        Entry::Vacant(entry) => {
                                            entry.insert((name, item.span));
                                        }
                                        Entry::Occupied(entry) => {
                                            self.double_import(
                                                &subitem.node,
                                                item.span,
                                                entry.get().1,
                                                module.name());
                                            continue;
                                        }
                                    }
                                    let mut is_ok = false;
                                    let as_item = Item::new(&subitem.node, Some(item_name));
                                    if exports.values.contains(&as_item) {
                                        imports.values.insert(&subitem.node, name);
                                        is_ok = true;
                                    }
                                    if exports.patterns.contains(&as_item) {
                                        imports.patterns.insert(&subitem.node, name);
                                        is_ok = true;
                                    }
                                    if !is_ok {
                                        self.subitem_not_exported(
                                            &subitem.node,
                                            &item.node.name.node,
                                            name,
                                            subitem.span,
                                            module.name());
                                    }
                                }
                            }
                            None => { }
                        }
                    }
                }
                None => { }
            }
        }
        
        imports
    }

    fn resolve(&mut self, module: &Module<RawSymbol>, ctx: &Context) -> Module<Symbol> {
        self.emit_errors = false;
        let locals = self.collect_items(module);
        self.emit_errors = true;
        let mut imports = self.collect_imports(module, &ctx);
        imports.add_locals(module.name(), &locals);
        let ctx = Context {
            imports: &imports,
            .. *ctx
        };
        let name = module.name();

        let mut decls = Vec::new();

        for decl in &module.items {
            let resolved = match decl.node {
                Decl::Impl(ref impl_) => {
                    Decl::Impl(self.resolve_impl(impl_, &ctx))
                }
                Decl::Infix(assoc, ref sym, ref precedence) => {
                    let sym_ = match sym.node {
                        RawSymbol::Unqualified(ref s) => s,
                        _ => panic!("infix should contain unqualified symbol"),
                    };
                    if imports.values.get(sym_.as_str()) == Some(&name) {
                        let new_sym = Symbol::Global(name.to_string(), sym_.to_string());
                        let sym_node = Node::new(new_sym, sym.span);
                        Decl::Infix(assoc, sym_node, precedence.clone())
                    } else {
                        self.unknown_symbol("local value", sym, name);
                        let sym_node = Node::new(Symbol::Unknown, sym.span);
                        Decl::Infix(assoc, sym_node, precedence.clone())
                    }
                }
                Decl::Let(LetDecl::Def(ref def)) => {
                    Decl::Let(LetDecl::Def(self.resolve_def(def, &ctx)))
                }
                Decl::Let(LetDecl::Type(ref type_)) => {
                    let sym = match type_.value.node {
                        RawSymbol::Unqualified(ref s) => s,
                        _ => panic!("annotation should contain unqualified symbol"),
                    };
                    let sym = if imports.values.get(sym.as_str()) == Some(&name) {
                        Symbol::Global(name.to_string(), sym.to_string())
                    } else {
                        self.unknown_symbol("local value", &type_.value, name);
                        Symbol::Unknown
                    };
                    let sym = Node::new(sym, type_.value.span);
                    let typ = self.resolve_scheme(&type_.type_, &ctx);
                    Decl::Let(LetDecl::Type(TypeAnnot {
                        value: sym,
                        type_: typ,
                    }))
                }
                Decl::Record(ref record) => {
                    Decl::Record(self.resolve_record(record, &ctx))
                }
                Decl::Trait(ref trait_) => {
                    Decl::Trait(self.resolve_trait(trait_, &ctx))
                }
                Decl::TypeAlias(ref alias) => {
                    Decl::TypeAlias(self.resolve_type_alias(alias, &ctx))
                }
                Decl::Union(ref union) => {
                    Decl::Union(self.resolve_union(union, &ctx))
                }
            };

            decls.push(Node::new(resolved, decl.span));
        }

        Module {
            items: decls,
            imports: Vec::new(),
            def: module.def.clone(),
        }
    }

    fn resolve_impl(
                    &mut self,
                    impl_: &Impl<RawSymbol>,
                    ctx: &Context) -> Impl<Symbol> {
        let scheme = self.resolve_scheme(&impl_.scheme, ctx);
        let trait_ = self.resolve_trait_bound(&impl_.trait_, ctx);
        let mut values = Vec::new();
        for value in &impl_.values {
            let resolved = self.resolve_def(&value.node, ctx);
            values.push(Node::new(resolved, value.span));
        }

        Impl {
            scheme: scheme,
            trait_: trait_,
            values: values,
        }
    }

    fn resolve_def(
                    &mut self,
                    def: &Def<RawSymbol>,
                    ctx: &Context) -> Def<Symbol> {
        let pattern = self.resolve_pattern(&def.pattern, ctx);
        let value = def.value.as_ref().map(|v| self.resolve_expr(v, ctx));
        Def {
            pattern: pattern,
            value: value,
        }
    }

    fn resolve_record(
                        &mut self,
                        record: &RecordType<RawSymbol>,
                        ctx: &Context) -> RecordType<Symbol> {
        let mut resolved_fields = Vec::new();
        for &(ref name, ref type_) in &record.fields {
            let resolved_type = self.resolve_type(type_, ctx);
            resolved_fields.push((name.clone(), resolved_type));
        }
        
        RecordType {
            name: record.name.clone(),
            vars: record.vars.clone(),
            fields: resolved_fields,
        }
    }

    fn resolve_trait(
                        &mut self,
                        trait_: &Trait<RawSymbol>,
                        ctx: &Context) -> Trait<Symbol> {
        let mut base_traits = Vec::new();
        for base in &trait_.base_traits {
            base_traits.push(self.resolve_trait_bound(base, ctx));
        }

        let mut values = Vec::new();
        for value in &trait_.values {
            let sym = match value.node.value.node {
                RawSymbol::Unqualified(ref s) => {
                    let s = Symbol::Global(ctx.module.to_string(), s.clone());
                    Node::new(s, value.node.value.span)
                }
                _ => {
                    panic!("type annotation should contain unqualified symbol");
                }
            };
            let typ = self.resolve_scheme(&value.node.type_, ctx);
            let annot = TypeAnnot {
                value: sym,
                type_: typ,
            };
            values.push(Node::new(annot, value.span));
        }

        Trait {
            name: trait_.name.clone(),
            vars: trait_.vars.clone(),
            base_traits: base_traits,
            values: values,
        }
    }

    fn resolve_type_alias(
                            &mut self,
                            alias: &TypeAlias<RawSymbol>,
                            ctx: &Context) -> TypeAlias<Symbol> {
        let resolved_type = alias.type_.as_ref().map(|t| {
            self.resolve_type(t, ctx)
        });
        TypeAlias {
            name: alias.name.clone(),
            vars: alias.vars.clone(),
            type_: resolved_type,
        }
    }

    fn resolve_union(
                        &mut self,
                        union: &UnionType<RawSymbol>,
                        ctx: &Context) -> UnionType<Symbol> {
        let mut resolved_cases = Vec::new();
        for case in &union.cases {
            let mut args = Vec::new();
            for arg in &case.node.args {
                args.push(self.resolve_type(arg, ctx));
            }
            let resolved_case = UnionCase {
                tag: case.node.tag.clone(),
                args: args,
            };
            resolved_cases.push(Node::new(resolved_case, case.span));
        }
        
        UnionType {
            name: union.name.clone(),
            vars: union.vars.clone(),
            cases: resolved_cases,
        }
    }

    fn resolve_type(
                    &mut self,
                    type_: &Node<Type<RawSymbol>>,
                    ctx: &Context) -> Node<Type<Symbol>> {
        let resolved = match type_.node {
            Type::Var(ref v) => Type::Var(v.clone()),
            Type::Concrete(ref symbol) => {
                let resolved = self.resolve_symbol(
                    &Node::new(symbol.clone(), type_.span),
                    "type",
                    ctx);
                Type::Concrete(resolved.node)
            }
            Type::Function(ref from, ref to) => {
                let from = self.resolve_type(from, ctx);
                let to = self.resolve_type(to, ctx);
                Type::Function(Box::new(from), Box::new(to))
            }
            Type::SelfType => Type::SelfType,
            Type::Apply(ref a, ref b) => {
                let a = self.resolve_type(a, ctx);
                let b = self.resolve_type(b, ctx);
                Type::Apply(Box::new(a), Box::new(b))
            }
        };

        Node::new(resolved, type_.span)
    }
    
    fn resolve_scheme(
                    &mut self,
                    type_: &Node<Scheme<RawSymbol>>,
                    ctx: &Context) -> Node<Scheme<Symbol>> {
        let mut bounds = Vec::new();
        for &(ref var, ref bound) in &type_.node.bounds {
            let bound = self.resolve_trait_bound(bound, ctx);
            bounds.push((var.clone(), bound));
        }

        let typ = self.resolve_type(&type_.node.type_, ctx);

        Node::new(Scheme {
            bounds: bounds,
            type_: typ,
        }, type_.span)
    }

    fn resolve_trait_bound(
                            &mut self,
                            bound: &Node<TraitBound<RawSymbol>>,
                            ctx: &Context) -> Node<TraitBound<Symbol>> {
        let trait_ = self.resolve_symbol(&bound.node.trait_, "trait", ctx);
        
        let mut params = Vec::new();
        for param in &bound.node.params {
            let typ = self.resolve_type(&param, ctx);
            params.push(typ);
        }
        
        Node::new(TraitBound {
            trait_: trait_,
            params: params,
        }, bound.span)
    }

    fn resolve_pattern(
                        &mut self,
                        pattern: &Node<Pattern<RawSymbol>>,
                        ctx: &Context) -> Node<Pattern<Symbol>> {
        unimplemented!()
    }

    fn resolve_expr(
                        &mut self,
                        pattern: &Node<Expr<RawSymbol>>,
                        ctx: &Context) -> Node<Expr<Symbol>> {
        unimplemented!()
    }

    fn resolve_symbol(
                        &mut self,
                        symbol: &Node<RawSymbol>,
                        kind: &str,
                        ctx: &Context) -> Node<Symbol> {
        let sym = match symbol.node {
            RawSymbol::Trusted(ref m, ref n) => {
                Symbol::Global(m.clone(), n.clone())
            }
            RawSymbol::Qualified(ref m, ref n) => {
                if let Some(m) = ctx.imports.modules.get(m.as_str()) {
                    if ctx.exports.get(*m).unwrap().has_type(n) {
                        Symbol::Global(m.to_string(), n.clone())
                    } else {
                        self.not_exported(n, m, symbol.span, ctx.module);
                        Symbol::Unknown
                    }
                } else {
                    self.unknown_module(m, symbol.span, ctx.module);
                    Symbol::Unknown
                }
            }
            RawSymbol::Unqualified(ref s) => {
                if let Some(m) = ctx.imports.types.get(s.as_str()) {
                    Symbol::Global(m.to_string(), s.clone())
                } else {
                    self.unknown_symbol(kind, symbol, ctx.module);
                    Symbol::Unknown
                }
            }
        };
        Node::new(sym, symbol.span)
    }
}
