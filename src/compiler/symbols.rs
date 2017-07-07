use std::collections::{HashMap, HashSet};
use std::collections::hash_map::Entry;
use std::fmt;
use ast::Node;
use ast::parsed as p;
use ast::parsed::{
    Module, Symbol, Decl, LetDecl, ItemList, Impl, Def, RecordType, Trait,
    TypeAlias, UnionType, Type, Expr, Pattern, Scheme, DoExpr,
};
use ast::resolved as r;
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

    fn has_symbol(&self, kind: Kind, sym: &str) -> bool {
        match kind {
            Kind::Value | Kind::LocalValue => self.has_value(sym),
            Kind::Pattern => self.has_pattern(sym),
            Kind::Trait => self.has_trait(sym),
            Kind::Type => self.has_type(sym),
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

    fn get_origin(&self, kind: Kind, symbol: &str) -> Option<&str> {
        let map = match kind {
            Kind::Value | Kind::LocalValue => &self.values,
            Kind::Pattern => &self.patterns,
            Kind::Trait => &self.traits,
            Kind::Type => &self.types,
        };
        map.get(symbol).map(|&x| x)
    }
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
enum Kind {
    LocalValue,
    Value,
    Pattern,
    Type,
    Trait,
}

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let as_str = match *self {
            Kind::LocalValue => "local value",
            Kind::Value => "value",
            Kind::Pattern => "pattern",
            Kind::Type => "type",
            Kind::Trait => "trait",
        };
        write!(f, "{}", as_str)
    }
}

pub fn resolve_symbols(modules: &HashMap<String, Module>) -> Result<r::Items, Vec<Error>> {
    let mut resolver = Resolver::new();
    let mut exports = HashMap::new();
    for (name, module) in modules {
        let module_exports = resolver.collect_exports(module);
        exports.insert(name.clone(), module_exports);
    }
    for (name, module) in modules {
        let ctx = Context {
            module: name,
            imports: &Imports::empty(),
            exports: &exports,
        };
        resolver.resolve(module, &ctx);
    }
    if resolver.errors.is_empty() {
        Ok(resolver.result)
    } else {
        Err(resolver.errors)
    }
}

struct Resolver {
    errors: Vec<Error>,
    emit_errors: bool,
    result: r::Items,
}

impl Resolver {
    fn new() -> Resolver {
        Resolver {
            errors: Vec::new(),
            emit_errors: true,
            result: r::Items::empty(),
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

    fn duplicate_binding(&mut self, name: &str, span: Span, previous: Span, module: &str) {
        if self.emit_errors {
            let message = format!("Name `{}` is bound multiple times.", name);
            let previous_message = "Note: previously bound here.";
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

    fn double_fixity_decl(&mut self, name: &str, span: Span, previous: Span, module: &str) {
        if self.emit_errors {
            let message = format!("Fixity of `{}` is declared twice.", name);
            let previous_message = "Note: previously declared here.";
            self.errors.push(errors::double_symbol_error(
                message,
                span,
                previous_message,
                previous,
                module));
        }
    }

    fn double_type_annotation(&mut self, name: &str, span: Span, previous: Span, module: &str) {
        if self.emit_errors {
            let message = format!("Type of `{}` is declared twice.", name);
            let previous_message = "Note: previously declared here.";
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

    fn unknown_symbol(&mut self, kind: Kind, symbol: &Node<p::Symbol>, module: &str) {
        if self.emit_errors {
            let message = format!("Unknown {}: `{}`.", kind, symbol.value.clone().full_name());
            self.errors.push(errors::symbol_error(message, symbol.span, module));
        }
    }

    fn unknown_local_symbol(&mut self, kind: Kind, symbol: &Node<String>, module: &str) {
        if self.emit_errors {
            let message = format!("Unknown local {}: `{}`.", kind, symbol.value);
            self.errors.push(errors::symbol_error(message, symbol.span, module));
        }
    }

    fn unknown_module(&mut self, m: &str, span: Span, module: &str) {
        if self.emit_errors {
            let message = format!("Unknown module: `{}`.", m);
            self.errors.push(errors::symbol_error(message, span, module));
        }
    }

    fn collect_items<'a>(&mut self, module: &'a Module) -> Exports<'a> {
        let mut values: HashMap<&'a str, (Option<&'a str>, Span)> = HashMap::new();
        let mut patterns: HashMap<&'a str, (Option<&'a str>, Span)> = HashMap::new();
        let mut traits: HashMap<&'a str, Span> = HashMap::new();
        let mut types: HashMap<&'a str, Span> = HashMap::new();

        for item in &module.items {
            match item.value {
                Decl::Let(LetDecl::Def(ref def)) => {
                    let vars = def.pattern.value.bound_vars(def.pattern.span);
                    for Node { value: var, span, .. } in vars {
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
                    let mut reported = false;
                    if let Some(span) = traits.get(record.name.value.as_str()) {
                        // duplicate type, defined at `record.name.span` and `span`
                        self.double_definition(
                            &record.name.value,
                            record.name.span,
                            *span,
                            module.name());
                        reported = true;
                    } else {
                        match types.entry(&record.name.value) {
                            Entry::Vacant(entry) => {
                                entry.insert(record.name.span);
                            }
                            Entry::Occupied(entry) => {
                                let span = entry.get();
                                // duplicate type, defined at `record.name.span` and `span`
                                self.double_definition(
                                    &record.name.value,
                                    record.name.span,
                                    *span,
                                    module.name());
                                reported = true;
                            }
                        }
                    }
                    match values.entry(&record.name.value) {
                        Entry::Vacant(entry) => {
                            entry.insert((None, record.name.span));
                        }
                        Entry::Occupied(entry) => {
                            if !reported {
                                let span = entry.get().1;
                                // duplicate value, defined at `record.name.span` and `span`
                                self.double_definition(
                                    &record.name.value,
                                    record.name.span,
                                    span,
                                    module.name());
                                reported = true;
                            }
                        }
                    }
                    match patterns.entry(&record.name.value) {
                        Entry::Vacant(entry) => {
                            entry.insert((None, record.name.span));
                        }
                        Entry::Occupied(entry) => {
                            if !reported {
                                let span = entry.get().1;
                                // duplicate pattern, defined at `record.name.span` and `span`
                                self.double_definition(
                                    &record.name.value,
                                    record.name.span,
                                    span,
                                    module.name());
                            }
                        }
                    }
                    for field in &record.fields {
                        match values.entry(&field.0.value) {
                            Entry::Vacant(entry) => {
                                entry.insert((Some(&record.name.value), field.0.span));
                            }
                            Entry::Occupied(entry) => {
                                let span = entry.get().1;
                                // duplicate value, defined at `field.0.span` and `span`
                                self.double_definition(
                                    &field.0.value,
                                    field.0.span,
                                    span,
                                    module.name());
                            }
                        }
                    }
                }
                Decl::Trait(ref trait_) => {
                    if let Some(span) = types.get(trait_.name.value.as_str()) {
                        // duplicate type, defined at `trait_.name.span` and `span`
                        self.double_definition(
                            &trait_.name.value,
                            trait_.name.span,
                            *span,
                            module.name());
                    } else {
                        match traits.entry(&trait_.name.value) {
                            Entry::Vacant(entry) => {
                                entry.insert(trait_.name.span);
                            }
                            Entry::Occupied(entry) => {
                                let span = entry.get();
                                // duplicate trait, defined at `trait_.name.span` and `span`
                                self.double_definition(
                                    &trait_.name.value,
                                    trait_.name.span,
                                    *span,
                                    module.name());
                            }
                        }
                    }

                    for value in &trait_.values {
                        let item = &value.value.value.value;
                        match values.entry(item) {
                            Entry::Vacant(entry) => {
                                entry.insert((None, value.value.value.span));
                            }
                            Entry::Occupied(entry) => {
                                let another = entry.get().1;
                                // duplicate value, defined at `span` and `another`
                                self.double_definition(
                                    item,
                                    value.value.value.span,
                                    another,
                                    module.name());
                            }
                        }
                    }
                }
                Decl::TypeAlias(ref type_) => {
                    if let Some(span) = traits.get(type_.name.value.as_str()) {
                        // duplicate type, defined at `type_.name.span` and `span`
                        self.double_definition(
                            &type_.name.value,
                            type_.name.span,
                            *span,
                            module.name());
                    } else {
                        match types.entry(&type_.name.value) {
                            Entry::Vacant(entry) => {
                                entry.insert(type_.name.span);
                            }
                            Entry::Occupied(entry) => {
                                let span = entry.get();
                                // duplicate type, defined at `type_.name.span` and `span`
                                self.double_definition(
                                    &type_.name.value,
                                    type_.name.span,
                                    *span,
                                    module.name());
                            }
                        }
                    }
                }
                Decl::Union(ref union) => {
                    match types.entry(&union.name.value) {
                        Entry::Vacant(entry) => {
                            entry.insert(union.name.span);
                        }
                        Entry::Occupied(entry) => {
                            let span = entry.get();
                            // duplicate type, defined at `span` and `union.name.span`
                            self.double_definition(
                                &union.name.value,
                                union.name.span,
                                *span,
                                module.name());
                        }
                    }
                    for case in &union.cases {
                        let reported = match values.entry(&case.value.tag.value) {
                            Entry::Vacant(entry) => {
                                entry.insert((Some(&union.name.value), case.value.tag.span));
                                false
                            }
                            Entry::Occupied(entry) => {
                                let span = entry.get().1;
                                // duplicate value, defined at `span` and `case.node.tag.span`
                                self.double_definition(
                                    &case.value.tag.value,
                                    case.value.tag.span,
                                    span,
                                    module.name());
                                true
                            }
                        };
                        match patterns.entry(&case.value.tag.value) {
                            Entry::Vacant(entry) => {
                                entry.insert((Some(&union.name.value), case.value.tag.span));
                            }
                            Entry::Occupied(entry) => {
                                if !reported {
                                    let span = entry.get().1;
                                    // duplicate pattern, defined at `span` and `case.node.tag.span`
                                    self.double_definition(
                                        &case.value.tag.value,
                                        case.value.tag.span,
                                        span,
                                        module.name());
                                }
                            }
                        }
                    }
                }
                Decl::Infix(_, _, _) |
                Decl::Impl(_) |
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

    fn collect_exports<'a>(&mut self, module: &'a Module) -> Exports<'a> {
        let items = self.collect_items(module);

        let exposed = match module.def.value.exposing.value {
            ItemList::All => return items,
            ItemList::Some(ref items) => items,
        };

        let mut result = Exports::empty();
        for item in exposed {
            let subitems = &item.value.subitems;
            let name = &item.value.name.value;
            let span = item.value.name.span;
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
                Some(Node { value: ItemList::All, .. }) => {
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
                Some(Node { value: ItemList::Some(ref subitems), .. }) => {
                    for item in subitems {
                        let item = Item::new(&item.value, Some(name));
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

    fn collect_imports<'a>(&mut self, module: &'a Module, ctx: &Context<'a>) -> Imports<'a> {
        let mut imports = Imports::empty();
        let mut import_span = HashMap::new();
        let mut first_import = HashMap::new();

        for import in &module.imports {
            let alias: &str = if let Some(ref alias) = import.value.alias {
                &alias.value
            } else {
                &import.value.name.value
            };

            match imports.modules.entry(alias) {
                Entry::Vacant(entry) => {
                    entry.insert(&import.value.name.value);
                    import_span.insert(alias, import.span);
                }
                Entry::Occupied(_) => {
                    let previous = import_span[alias];
                    self.module_double_import(alias, import.value.name.span, previous, module.name());
                    continue;
                }
            }

            let name = &import.value.name.value;

            let exports = if let Some(exports) = ctx.exports.get(name) {
                exports
            } else {
                panic!("all required modules should be available at symbol resolution");
            };

            match import.value.exposing {
                Some(Node { value: ItemList::All, span: list_span, .. }) => {
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
                Some(Node { value: ItemList::Some(ref items), .. }) => {
                    for item in items {
                        let item_name = &item.value.name.value;
                        let item_span = item.value.name.span;
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
                        match item.value.subitems {
                            Some(Node { value: ItemList::All, span: list_span, .. }) => {
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
                                        &item.value.name.value,
                                        name,
                                        item.value.name.span,
                                        module.name());
                                }
                            }
                            Some(Node { value: ItemList::Some(ref items), .. }) => {
                                for subitem in items {
                                    match first_import.entry(&subitem.value) {
                                        Entry::Vacant(entry) => {
                                            entry.insert((name, item.span));
                                        }
                                        Entry::Occupied(entry) => {
                                            self.double_import(
                                                &subitem.value,
                                                item.span,
                                                entry.get().1,
                                                module.name());
                                            continue;
                                        }
                                    }
                                    let mut is_ok = false;
                                    let as_item = Item::new(&subitem.value, Some(item_name));
                                    if exports.values.contains(&as_item) {
                                        imports.values.insert(&subitem.value, name);
                                        is_ok = true;
                                    }
                                    if exports.patterns.contains(&as_item) {
                                        imports.patterns.insert(&subitem.value, name);
                                        is_ok = true;
                                    }
                                    if !is_ok {
                                        self.subitem_not_exported(
                                            &subitem.value,
                                            &item.value.name.value,
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

    fn resolve(&mut self, module: &Module, ctx: &Context) {
        self.emit_errors = false;
        let locals = self.collect_items(module);
        self.emit_errors = true;
        let mut imports = self.collect_imports(module, ctx);
        imports.add_locals(module.name(), &locals);
        let ctx = Context {
            imports: &imports,
            .. *ctx
        };
        let name = module.name();

        let mut fixity_decl = HashMap::new();
        let mut type_annotation = HashMap::new();
        for decl in &module.items {
            match decl.value {
                Decl::Impl(ref impl_) => {
                    let impl_ = self.resolve_impl(impl_, &ctx);
                    self.result.impls.push(impl_);
                }
                Decl::Infix(assoc, ref sym, ref prec) => {
                    let sym_ = &sym.value;
                    if imports.values.get(sym_.as_str()) == Some(&name) {
                        match fixity_decl.entry(sym_.as_str()) {
                            Entry::Vacant(entry) => {
                                entry.insert(decl.span);
                                let name = format!("{}.{}", name, sym.value);
                                self.result.fixities.insert(name, (assoc, prec.value));
                            }
                            Entry::Occupied(entry) => {
                                let previous = *entry.get();
                                self.double_fixity_decl(
                                    sym_,
                                    sym.span,
                                    previous,
                                    name);
                            }
                        }
                    } else {
                        let sym = sym.clone().map(|s| Symbol::Qualified(name.into(), s));
                        self.unknown_symbol(Kind::LocalValue, &sym, name);
                    }
                }
                Decl::Let(LetDecl::Def(ref def)) => {
                    let def = self.resolve_def(def, &ctx);
                    self.result.items.push(def);
                }
                Decl::Let(LetDecl::Type(ref type_)) => {
                    let sym = type_.value.value.as_str();
                    if imports.values.get(sym) == Some(&name) {
                        let full_sym = format!("{}.{}", name, sym);
                        let sym_node = Node::new(full_sym.clone(), type_.value.span);
                        let rtype = type_.type_.as_ref().map(|t| {
                            self.resolve_scheme(t, &ctx)
                        });
                        let annotation = r::TypeAnnot {
                            value: sym_node,
                            type_: rtype,
                            module: name.into(),
                        };
                        match type_annotation.entry(sym) {
                            Entry::Vacant(entry) => {
                                entry.insert(type_.value.span);
                                self.result.annotations.insert(full_sym, annotation);
                            }
                            Entry::Occupied(entry) => {
                                let previous = *entry.get();
                                self.double_type_annotation(
                                    sym,
                                    type_.value.span,
                                    previous,
                                    name);
                            }
                        }
                    } else {
                        self.unknown_local_symbol(Kind::Value, &type_.value, name);
                    }
                }
                Decl::Record(ref record) => {
                    let type_ = self.resolve_record(record, &ctx);
                    self.result.types.push(r::TypeDecl::Record(type_));
                }
                Decl::Trait(ref trait_) => {
                    let trait_ = self.resolve_trait(trait_, &ctx);
                    self.result.traits.push(trait_);
                }
                Decl::TypeAlias(ref alias) => {
                    let type_ = self.resolve_type_alias(alias, &ctx);
                    self.result.types.push(r::TypeDecl::TypeAlias(type_));
                }
                Decl::Union(ref union) => {
                    let type_ = self.resolve_union(union, &ctx);
                    self.result.types.push(r::TypeDecl::Union(type_));
                }
            };
        }
    }

    fn resolve_impl(
                    &mut self,
                    impl_: &Impl,
                    ctx: &Context) -> r::Impl {
        let scheme = self.resolve_scheme(&impl_.scheme, ctx);
        let trait_ = self.resolve_trait_bound(&impl_.trait_, ctx);
        let mut values = Vec::new();
        for value in &impl_.values {
            let resolved = self.resolve_def(&value.value, ctx);
            values.push(Node::new(resolved, value.span));
        }

        r::Impl {
            scheme: scheme,
            trait_: trait_,
            values: values,
            module: ctx.module.into(),
        }
    }

    fn resolve_def(
                    &mut self,
                    def: &Def,
                    ctx: &Context) -> r::Def {
        let pattern = self.resolve_pattern(&def.pattern, ctx);
        let value = def.value.as_ref().map(|v| self.resolve_expr(v, ctx));
        r::Def {
            pattern: pattern,
            value: value,
            module: ctx.module.to_string(),
        }
    }

    fn resolve_record(
                        &mut self,
                        record: &RecordType,
                        ctx: &Context) -> r::RecordType {
        let mut resolved_fields = Vec::new();
        for &(ref name, ref type_) in &record.fields {
            let resolved_type = self.resolve_type(type_, ctx);
            resolved_fields.push((name.clone(), resolved_type));
        }
        
        let name = format!("{}.{}", ctx.module, record.name.value);
        r::RecordType {
            name: Node::new(name, record.name.span),
            vars: record.vars.clone(),
            fields: resolved_fields,
            module: ctx.module.into(),
        }
    }

    fn resolve_trait(
                        &mut self,
                        trait_: &Trait,
                        ctx: &Context) -> r::Trait {
        let mut base_traits = Vec::new();
        for base in &trait_.base_traits {
            base_traits.push(self.resolve_trait_bound(base, ctx));
        }

        let mut values = Vec::new();
        for value in &trait_.values {
            let sym = r::Symbol::Global(format!("{}.{}", ctx.module, value.value.value.value));
            let sym = Node::new(sym, value.value.value.span);
            let typ = value.value.type_.as_ref().map(|t| {
                self.resolve_scheme(t, ctx)
            });
            let annot = r::TypeAnnot {
                value: sym.map(r::Symbol::full_name),
                type_: typ,
                module: ctx.module.into(),
            };
            values.push(Node::new(annot, value.span));
        }

        let name = format!("{}.{}", ctx.module, trait_.name.value);
        r::Trait {
            name: Node::new(name, trait_.name.span),
            base_traits: base_traits,
            values: values,
            module: ctx.module.into(),
        }
    }

    fn resolve_type_alias(
                            &mut self,
                            alias: &TypeAlias,
                            ctx: &Context) -> r::TypeAlias {
        let resolved_type = alias.type_.as_ref().map(|t| {
            self.resolve_type(t, ctx)
        });

        let name = format!("{}.{}", ctx.module, alias.name.value);
        r::TypeAlias {
            name: Node::new(name, alias.name.span),
            vars: alias.vars.clone(),
            type_: resolved_type,
            module: ctx.module.into(),
        }
    }

    fn resolve_union(
                        &mut self,
                        union: &UnionType,
                        ctx: &Context) -> r::UnionType {
        let mut resolved_cases = Vec::new();
        for case in &union.cases {
            let mut args = Vec::new();
            for arg in &case.value.args {
                args.push(self.resolve_type(arg, ctx));
            }
            let resolved_case = r::UnionCase {
                tag: case.value.tag.clone(),
                args: args,
            };
            resolved_cases.push(Node::new(resolved_case, case.span));
        }
        
        let name = format!("{}.{}", ctx.module, union.name.value);
        r::UnionType {
            name: Node::new(name, union.name.span),
            vars: union.vars.clone(),
            cases: resolved_cases,
            module: ctx.module.into(),
        }
    }

    fn resolve_type(
                    &mut self,
                    type_: &Node<Type>,
                    ctx: &Context) -> Node<r::Type> {
        let resolved = match type_.value {
            Type::Var(ref v) => r::Type::Var(v.clone()),
            Type::Concrete(ref symbol) => {
                let resolved = self.resolve_symbol(
                    &Node::new(symbol.clone(), type_.span),
                    Kind::Type,
                    ctx);
                r::Type::Concrete(resolved.value.full_name())
            }
            Type::Function(ref from, ref to) => {
                let from = self.resolve_type(from, ctx);
                let to = self.resolve_type(to, ctx);
                r::Type::Function(Box::new(from), Box::new(to))
            }
            Type::SelfType => r::Type::SelfType,
            Type::Apply(ref a, ref b) => {
                let a = self.resolve_type(a, ctx);
                let b = self.resolve_type(b, ctx);
                r::Type::Apply(Box::new(a), Box::new(b))
            }
            Type::Tuple(ref items) => {
                r::Type::Tuple(items.iter().map(|t| self.resolve_type(t, ctx)).collect())
            }
        };

        Node::new(resolved, type_.span)
    }
    
    fn resolve_scheme(
                    &mut self,
                    type_: &Node<Scheme>,
                    ctx: &Context) -> Node<r::Scheme> {
        let mut bounds = Vec::new();
        for &(ref var, ref bound) in &type_.value.bounds {
            let bound = self.resolve_trait_bound(bound, ctx);
            bounds.push((var.clone(), bound));
        }

        let typ = self.resolve_type(&type_.value.type_, ctx);

        Node::new(r::Scheme {
            bounds: bounds,
            type_: typ,
        }, type_.span)
    }

    fn resolve_trait_bound(
                            &mut self,
                            bound: &Node<Symbol>,
                            ctx: &Context) -> Node<String> {
        self.resolve_symbol(bound, Kind::Trait, ctx).map(r::Symbol::full_name)
    }

    fn resolve_pattern(
                        &mut self,
                        pattern: &Node<Pattern>,
                        ctx: &Context) -> Node<r::Pattern> {
        let resolved = match pattern.value {
            Pattern::Wildcard => r::Pattern::Wildcard,
            Pattern::Var(ref s) => r::Pattern::Var(s.clone()),
            Pattern::Literal(ref lit) => r::Pattern::Literal(lit.clone()),
            Pattern::Deconstruct(ref sym, ref items) => {
                let s = self.resolve_symbol(sym, Kind::Pattern, ctx).map(r::Symbol::full_name);
                let items = items.iter().map(|p| self.resolve_pattern(p, ctx)).collect();
                r::Pattern::Deconstruct(s, items)
            }
            Pattern::Infix(ref lhs, ref sym, ref rhs) => {
                let lhs = self.resolve_pattern(lhs, ctx);
                let rhs = self.resolve_pattern(rhs, ctx);
                let s = self.resolve_symbol(sym, Kind::Pattern, ctx);
                r::Pattern::Infix(Box::new(lhs), s, Box::new(rhs))
            }
            Pattern::As(ref pat, ref alias) => {
                let pat = self.resolve_pattern(pat, ctx);
                r::Pattern::As(Box::new(pat), alias.clone())
            }
            Pattern::Parenthesised(ref pat) => {
                let pat = self.resolve_pattern(pat, ctx);
                r::Pattern::Parenthesised(Box::new(pat))
            }
            Pattern::Tuple(ref items) => {
                let items = items.iter().map(|p| self.resolve_pattern(p, ctx)).collect();
                r::Pattern::Tuple(items)
            }
            Pattern::List(ref items) => {
                let items = items.iter().map(|p| self.resolve_pattern(p, ctx)).collect();
                r::Pattern::List(items)
            }
        };
        Node::new(resolved, pattern.span)
    }

    fn resolve_expr(
                        &mut self,
                        expr: &Node<Expr>,
                        ctx: &Context) -> Node<r::Expr> {
        let mut locals = Vec::new();
        self.resolve_expr_with_locals(expr, ctx, &mut locals)
    }

    fn resolve_expr_with_locals<'a>(
                                    &mut self,
                                    expr: &'a Node<Expr>,
                                    ctx: &Context,
                                    locals: &mut Vec<Node<&'a str>>) -> Node<r::Expr> {
        let resolved = match expr.value {
            Expr::Apply(ref a, ref b) => {
                let a = self.resolve_expr_with_locals(a, ctx, locals);
                let b = self.resolve_expr_with_locals(b, ctx, locals);
                r::Expr::Apply(Box::new(a), Box::new(b))
            }
            Expr::Case(ref value, ref arms) => {
                let value = self.resolve_expr_with_locals(value, ctx, locals);
                let mut resolved_arms = Vec::new();
                let locals_before = locals.len();
                for arm in arms {
                    let pat = self.resolve_pattern(&arm.value.pattern, ctx);
                    arm.value.pattern.value.collect_vars(locals, arm.value.pattern.span);
                    self.check_dupe_bindings(&locals[locals_before..], ctx);
                    let guard = arm.value.guard.as_ref().map(|e| {
                        self.resolve_expr_with_locals(e, ctx, locals)
                    });
                    let value = self.resolve_expr_with_locals(&arm.value.value, ctx, locals);
                    let res = r::CaseBranch {
                        pattern: pat,
                        guard: guard,
                        value: value,
                    };
                    resolved_arms.push(Node::new(res, arm.span));
                    while locals.len() > locals_before {
                        locals.pop();
                    }
                }
                r::Expr::Case(Box::new(value), resolved_arms)
            }
            Expr::Do(ref do_) => {
                r::Expr::Do(Box::new(self.resolve_do(do_, ctx, locals)))
            }
            Expr::Ident(ref ident) => {
                let node = Node::new(ident.clone(), expr.span);
                let resolved = self.resolve_symbol_in_expr(&node, ctx, locals);
                r::Expr::Ident(resolved.value)
            }
            Expr::If(ref cond, ref then, ref else_) => {
                let cond = self.resolve_expr_with_locals(cond, ctx, locals);
                let then = self.resolve_expr_with_locals(then, ctx, locals);
                let else_ = self.resolve_expr_with_locals(else_, ctx, locals);
                r::Expr::If(Box::new(cond), Box::new(then), Box::new(else_))
            }
            Expr::Infix(ref lhs, ref sym, ref rhs) => {
                let lhs = self.resolve_expr_with_locals(lhs, ctx, locals);
                let rhs = self.resolve_expr_with_locals(rhs, ctx, locals);
                let sym = self.resolve_symbol_in_expr(sym, ctx, locals);
                r::Expr::Infix(Box::new(lhs), sym, Box::new(rhs))
            }
            Expr::Lambda(ref params, ref value) => {
                let locals_before = locals.len();
                let mut resolved_params = Vec::new();
                for param in params {
                    resolved_params.push(self.resolve_pattern(param, ctx));
                    param.value.collect_vars(locals, param.span);
                }
                self.check_dupe_bindings(&locals[locals_before..], ctx);
                let value = self.resolve_expr_with_locals(value, ctx, locals);
                while locals.len() > locals_before {
                    locals.pop();
                }
                r::Expr::Lambda(resolved_params, Box::new(value))
            }
            Expr::Let(ref defs, ref value) => {
                self.resolve_let(defs, value, ctx, locals)
            }
            Expr::List(ref items) => {
                let items = items
                    .iter()
                    .map(|e| self.resolve_expr_with_locals(e, ctx, locals))
                    .collect();
                r::Expr::List(items)
            }
            Expr::Literal(ref literal) => {
                r::Expr::Literal(literal.clone())
            }
            Expr::Parenthesised(ref expr) => {
                let expr = self.resolve_expr_with_locals(expr, ctx, locals);
                r::Expr::Parenthesised(Box::new(expr))
            }
            Expr::Tuple(ref items) => {
                let items = items
                    .iter()
                    .map(|e| self.resolve_expr_with_locals(e, ctx, locals))
                    .collect();
                r::Expr::Tuple(items)
            }
        };
        Node::new(resolved, expr.span)
    }

    fn resolve_do<'a>(
                        &mut self,
                        expr: &'a Node<DoExpr>,
                        ctx: &Context,
                        locals: &mut Vec<Node<&'a str>>) -> Node<r::DoExpr> {
        let resolved = match expr.value {
            DoExpr::Done(ref expr) => {
                let expr = self.resolve_expr_with_locals(expr, ctx, locals);
                r::DoExpr::Done(expr)
            }
            DoExpr::Bind(ref pat, ref expr, ref rest) => {
                let expr = self.resolve_expr_with_locals(expr, ctx, locals);
                let locals_before = locals.len();
                pat.value.collect_vars(locals, pat.span);
                self.check_dupe_bindings(&locals[locals_before..], ctx);
                let pat = self.resolve_pattern(pat, ctx);
                let rest = self.resolve_do(rest, ctx, locals);
                while locals.len() > locals_before {
                    locals.pop();
                }
                r::DoExpr::Bind(pat, expr, Box::new(rest))
            }
            DoExpr::If(ref cond, ref rest) => {
                let cond = self.resolve_expr_with_locals(cond, ctx, locals);
                let rest = self.resolve_do(rest, ctx, locals);
                r::DoExpr::If(cond, Box::new(rest))
            }
            DoExpr::Let(ref pat, ref val, ref rest) => {
                let locals_before = locals.len();
                pat.value.collect_vars(locals, pat.span);
                self.check_dupe_bindings(&locals[locals_before..], ctx);
                let pat = self.resolve_pattern(pat, ctx);
                let val = self.resolve_expr_with_locals(val, ctx, locals);
                let rest = self.resolve_do(rest, ctx, locals);
                while locals.len() > locals_before {
                    locals.pop();
                }
                r::DoExpr::Let(pat, val, Box::new(rest))
            }
            DoExpr::Sequence(ref expr, ref rest) => {
                let expr = self.resolve_expr_with_locals(expr, ctx, locals);
                let rest = self.resolve_do(rest, ctx, locals);
                r::DoExpr::If(expr, Box::new(rest))
            }
        };

        Node::new(resolved, expr.span)
    }

    fn resolve_let<'a>(
                        &mut self,
                        decls: &'a [Node<LetDecl>],
                        value: &'a Node<Expr>,
                        ctx: &Context,
                        locals: &mut Vec<Node<&'a str>>) -> r::Expr {
        let mut defined_symbols = Vec::new();
        let mut resolved_defs = Vec::new();
        let mut resolved_types = Vec::new();
        let mut annotation_pos = HashMap::new();
        for decl in decls {
            if let LetDecl::Def(ref def) = decl.value {
                def.pattern.value.collect_vars(&mut defined_symbols, def.pattern.span);
            }
        }
        self.check_dupe_bindings(&defined_symbols, ctx);
        for decl in decls {
            if let LetDecl::Type(ref type_annot) = decl.value {
                match annotation_pos.entry(type_annot.value.value.clone()) {
                    Entry::Vacant(entry) => {
                        entry.insert(type_annot.value.span);
                    }
                    Entry::Occupied(entry) => {
                        self.double_type_annotation(
                            &type_annot.value.value,
                            type_annot.value.span,
                            *entry.get(),
                            ctx.module);
                    }
                }
                if defined_symbols.iter().any(|s| s.value == type_annot.value.value) {
                    let type_ = type_annot.type_.as_ref().map(|t| {
                        self.resolve_scheme(t, ctx)
                    });
                    let annot = r::TypeAnnot {
                        value: type_annot.value.clone(),
                        type_: type_,
                        module: String::new(),
                    };
                    resolved_types.push(Node::new(annot, decl.span));
                } else {
                    self.unknown_local_symbol(Kind::Value, &type_annot.value, ctx.module);
                }
            }
        }
        let locals_before = locals.len();
        locals.extend(defined_symbols);
        for decl in decls {
            if let LetDecl::Def(ref def) = decl.value {
                let pat = self.resolve_pattern(&def.pattern, ctx);
                let value = def.value.as_ref().map(|v| {
                    self.resolve_expr_with_locals(v, ctx, locals)
                });
                let def = r::Def {
                    pattern: pat,
                    value: value,
                    module: ctx.module.to_string(),
                };
                resolved_defs.push(Node::new(def, decl.span));
            }
        }
        let value = self.resolve_expr_with_locals(value, ctx, locals);
        while locals.len() > locals_before {
            locals.pop();
        }
        r::Expr::Let(resolved_defs, resolved_types, Box::new(value))
    }
    
    fn check_dupe_bindings(
                            &mut self, 
                            locals: &[Node<&str>],
                            ctx: &Context) {
        for (index, first) in locals.iter().enumerate() {
            for second in locals.iter().skip(index + 1) {
                if first.value == second.value {
                    self.duplicate_binding(
                        second.value,
                        second.span,
                        first.span,
                        ctx.module);
                }
            }
        }
    }

    fn resolve_symbol(
                        &mut self,
                        symbol: &Node<Symbol>,
                        kind: Kind,
                        ctx: &Context) -> Node<r::Symbol> {
        let sym = match symbol.value {
            Symbol::Qualified(ref m, ref n) => {
                if let Some(m) = ctx.imports.modules.get(m.as_str()) {
                    if ctx.exports.get(*m).unwrap().has_symbol(kind, n) {
                        r::Symbol::Global(format!("{}.{}", m, n))
                    } else {
                        self.not_exported(n, m, symbol.span, ctx.module);
                        r::Symbol::Unknown
                    }
                } else {
                    self.unknown_module(m, symbol.span, ctx.module);
                    r::Symbol::Unknown
                }
            }
            Symbol::Unqualified(ref s) => {
                if let Some(m) = ctx.imports.get_origin(kind, s) {
                    r::Symbol::Global(format!("{}.{}", m, s))
                } else {
                    self.unknown_symbol(kind, symbol, ctx.module);
                    r::Symbol::Unknown
                }
            }
        };
        Node::new(sym, symbol.span)
    }

    fn resolve_symbol_in_expr(
                                &mut self,
                                symbol: &Node<Symbol>,
                                ctx: &Context,
                                locals: &[Node<&str>]) -> Node<r::Symbol> {
        if let Symbol::Unqualified(ref name) = symbol.value {
            if locals.iter().any(|n| n.value == name) {
                let s = r::Symbol::Local(name.clone());
                return Node::new(s, symbol.span);
            }
        }
        self.resolve_symbol(symbol, Kind::Value, ctx)
    }
}
