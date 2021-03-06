use std::collections::BTreeMap;
use std::collections::btree_map::Entry;
use std::fmt;
use codemap::Span;
use ast::{Node, Sym, Symbol};
use ast::parsed::{
    self as p, Module, Decl, LetDecl, ItemList, Impl, Def, Trait,
    TypeAlias, UnionType, Type, Expr, Pattern, Scheme, DoExpr, ModuleDef,
};
use ast::resolved as r;
use CompileCtx;


#[derive(Debug, Clone, Default)]
struct Exports<'a> {
    types: BTreeMap<&'a str, Sym>,
    traits: BTreeMap<&'a str, Sym>,
    patterns: BTreeMap<&'a str, Sym>,
    values: BTreeMap<&'a str, Sym>,
    pattern_parents: BTreeMap<&'a str, &'a str>,
    value_parents: BTreeMap<&'a str, &'a str>,
}

impl<'a> Exports<'a> {
    fn empty() -> Exports<'a> {
        Default::default()
    }

    fn get_symbol(&self, kind: Kind, sym: &str) -> Option<Sym> {
        match kind {
            Kind::Value | Kind::LocalValue => self.get_value(sym),
            Kind::Pattern => self.get_pattern(sym),
            Kind::Trait => self.get_trait(sym),
            Kind::Type => self.get_type(sym),
        }
    }

    fn get_type(&self, type_: &str) -> Option<Sym> {
        self.types.get(&type_).cloned()
    }

    fn get_trait(&self, trait_: &str) -> Option<Sym> {
        self.traits.get(&trait_).cloned()
    }

    fn get_pattern(&self, pattern: &str) -> Option<Sym> {
        self.patterns.get(&pattern).cloned()
    }

    fn get_value(&self, value: &str) -> Option<Sym> {
        self.values.get(&value).cloned()
    }

    fn get_value_with_parent(&self, value: &str, parent: Option<&str>) -> Option<Sym> {
        if let Some(sym) = self.get_value(value) {
            if self.value_parents.get(value).cloned() == parent {
                Some(sym)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn get_pattern_with_parent(&self, pattern: &str, parent: Option<&str>) -> Option<Sym> {
        if let Some(sym) = self.get_pattern(pattern) {
            if self.pattern_parents.get(pattern).cloned() == parent {
                Some(sym)
            } else {
                None
            }
        } else {
            None
        }
    }
}

#[derive(Debug, Default)]
struct Imports<'a> {
    modules: BTreeMap<&'a str, &'a str>,
    types: BTreeMap<&'a str, &'a str>,
    traits: BTreeMap<&'a str, &'a str>,
    patterns: BTreeMap<&'a str, &'a str>,
    values: BTreeMap<&'a str, &'a str>,
}

impl<'a> Imports<'a> {
    fn empty() -> Imports<'a> {
        Default::default()
    }

    fn add_locals(&mut self, my_name: &'a str, locals: &Exports<'a>) {
        for type_ in locals.types.keys() {
            self.types.insert(*type_, my_name);
        }
        for trait_ in locals.traits.keys() {
            self.traits.insert(*trait_, my_name);
        }
        for pattern in locals.patterns.keys() {
            self.patterns.insert(*pattern, my_name);
        }
        for value in locals.values.keys() {
            self.values.insert(*value, my_name);
        }
    }

    fn get_origin(&self, kind: Kind, symbol: &str) -> Option<&str> {
        let map = match kind {
            Kind::Value | Kind::LocalValue => &self.values,
            Kind::Pattern => &self.patterns,
            Kind::Trait => &self.traits,
            Kind::Type => &self.types,
        };
        map.get(symbol).cloned()
    }
}

#[derive(Debug)]
struct Context<'a> {
    module: String,
    locals: &'a Exports<'a>,
    imports: &'a Imports<'a>,
    exports: &'a BTreeMap<String, Exports<'a>>,
}

#[derive(Default)]
struct TraitInfo {
    item_symbols: BTreeMap<String, Sym>,
}

impl TraitInfo {
    fn new() -> Self {
        Default::default()
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Copy, Hash, Clone)]
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

pub(crate) fn resolve_symbols(modules: &BTreeMap<String, Module>, ctx: &mut CompileCtx) -> r::Items {
    let mut resolver = Resolver::new(ctx);
    let mut items = BTreeMap::new();
    let mut exports = BTreeMap::new();
    for (name, module) in modules {
        let module_items = resolver.collect_items(module);
        let module_exports = resolver.collect_exports(module, &module_items);
        items.insert(name.clone(), module_items);
        exports.insert(name.clone(), module_exports);
    }
    for (name, module) in modules {
        let ctx = Context {
            module: name.clone(),
            locals: &items[name.as_str()],
            imports: &Imports::empty(),
            exports: &exports,
        };
        resolver.resolve(module, &ctx);
    }
    resolver.result
}

struct Resolver<'a> {
    ctx: &'a mut CompileCtx,
    result: r::Items,
    traits: BTreeMap<Sym, TraitInfo>,
}

impl<'a> Resolver<'a> {
    fn new(ctx: &'a mut CompileCtx) -> Resolver<'a> {
        Resolver {
            ctx,
            result: r::Items::new(),
            traits: BTreeMap::new(),
        }
    }

    fn fresh_sym<S: Into<String>>(&mut self, name: S) -> Sym {
        self.ctx.symbols.fresh_sym(name.into())
    }

    fn fresh_type_sym(&mut self, module: &str, name: &str) -> Sym {
        use compiler::builtins::types;
        match (module, name) {
            ("Basics", "Frac") => types::FRAC,
            ("Basics", "Bool") => types::BOOL,
            ("Basics", "Char") => types::CHAR,
            ("String", "String") => types::STRING,
            ("List", "List") => types::LIST,
            ("Basics", "Int") => types::INT,
            _ => {
                self.fresh_sym(name)
            }
        }
    }

    fn fresh_trait_sym(&mut self, module: &str, name: &str) -> Sym {
        use compiler::builtins::traits;
        match (module, name) {
            ("Computation", "Computation") => traits::COMPUTATION,
            ("Computation", "Failable") => traits::FAILABLE,
            ("Basics", "Default") => traits::DEFAULT,
            ("Basics", "Eq") => traits::EQ,
            ("Basics", "Ord") => traits::ORD,
            ("Basics", "ToString") => traits::TO_STRING,
            _ => {
                //let name = name.into();
                self.fresh_sym(name)
            }
        }
    }

    fn fresh_value_sym(&mut self, module: &str, name: &str, parent: Option<&str>) -> Sym {
        use compiler::builtins::values;
        match (module, parent, name) {
            ("List", Some("List"), "Nil") => values::NIL,
            ("List", Some("List"), "::") => values::CONS,
            ("Option", Some("Option"), "Some") => values::SOME,
            ("Option", Some("Option"), "None") => values::NONE,
            ("Computation", None, "andThen") => values::AND_THEN,
            ("Computation", None, "fail") => values::FAIL,
            ("Basics", None, "default") => values::DEFAULT,
            ("Basics", None, "&&") => values::AND,
            ("Basics", None, "||") => values::OR,
            ("Basics", None, "intAdd") => values::INT_ADD,
            ("Basics", None, "intSub") => values::INT_SUB,
            ("Basics", None, "intMul") => values::INT_MUL,
            ("Basics", None, "intDiv") => values::INT_DIV,
            ("Basics", None, "intEq") => values::INT_EQ,
            ("Basics", None, "intCmp") => values::INT_CMP,
            ("Basics", None, "intToString") => values::INT_TO_STR,
            ("Basics", None, "fracAdd") => values::FRAC_ADD,
            ("Basics", None, "fracSub") => values::FRAC_SUB,
            ("Basics", None, "fracMul") => values::FRAC_MUL,
            ("Basics", None, "fracDiv") => values::FRAC_DIV,
            ("Basics", None, "fracEq") => values::FRAC_EQ,
            ("Basics", None, "fracCmp") => values::FRAC_CMP,
            ("Basics", None, "fracToString") => values::FRAC_TO_STR,
            ("Basics", None, "charToString") => values::CHAR_TO_STR,
            ("Basics", None, "charEq") => values::CHAR_EQ,
            ("Basics", None, "charCmp") => values::CHAR_CMP,
            ("Basics", Some("Ordering"), "LT") => values::LT,
            ("Basics", Some("Ordering"), "EQ") => values::EQ,
            ("Basics", Some("Ordering"), "GT") => values::GT,
            ("String", None, "append") => values::STR_APPEND,
            ("String", None, "charAt") => values::STR_CHAR_AT,
            ("String", None, "length") => values::STR_LENGTH,
            ("String", None, "substring") => values::STR_SUBSTRING,
            ("String", None, "cmp") => values::STR_CMP,
            ("Main", None, "main") => values::MAIN,
            _ => self.fresh_sym(name),
        }
    }

    fn fresh_var_sym(&mut self, var: &str) -> Sym {
        self.fresh_sym(var)
    }

    fn fresh_artificial_sym(&mut self) -> Sym {
        self.ctx.symbols.fresh_artificial_sym()
    }

    fn double_definition(&mut self, name: &str, span: Span, previous: Span) {
        let message = format!("Item `{}` is defined multiple times.", name);
        let previous_message = "Note: previously defined here.";
        self.ctx.reporter
            .symbol_error(message.as_str(), span)
            .span_note(message, span)
            .span_note(previous_message, previous)
            .done();
    }

    fn duplicate_binding(&mut self, name: &str, span: Span, previous: Span) {
        let message = format!("Name `{}` is bound multiple times.", name);
        let previous_message = "Note: previously bound here.";
        self.ctx.reporter
            .symbol_error(message.as_str(), span)
            .span_note(message, span)
            .span_note(previous_message, previous)
            .done();
    }

    fn bad_export(&mut self, message: String, span: Span) {
        self.ctx.reporter
            .symbol_error(message.as_str(), span)
            .span_note(message, span)
            .done();
    }

    fn module_double_import(&mut self, offending: &str, span: Span, previous: Span) {
        let message = format!("Module `{}` is imported twice.", offending);
        let previous_message = "Note: previously imported here.";
        self.ctx.reporter
            .symbol_error(message.as_str(), span)
            .span_note(message, span)
            .span_note(previous_message, previous)
            .done();
    }

    fn double_import(&mut self, kind: Kind, item: &str, span: Span, previous: Span) {
        let message = format!("{} `{}` is imported multiple times.", kind, item);
        let previous_message = "Note: previously imported here.";
        self.ctx.reporter
            .symbol_error(message.as_str(), span)
            .span_note(message, span)
            .span_note(previous_message, previous)
            .done();
    }

    fn double_export(&mut self, kind: Kind, item: &str, span: Span, previous: Span) {
        let message = format!("{} `{}` is exported multiple times.", kind, item);
        let previous_message = "Note: previously exported here.";
        self.ctx.reporter
            .symbol_error(message.as_str(), span)
            .span_note(message, span)
            .span_note(previous_message, previous)
            .done();
    }

    fn double_fixity_decl(&mut self, name: &str, span: Span, previous: Span) {
        let message = format!("Fixity of `{}` is declared twice.", name);
        let previous_message = "Note: previously declared here.";
        self.ctx.reporter
            .symbol_error(message.as_str(), span)
            .span_note(message, span)
            .span_note(previous_message, previous)
            .done();
    }

    fn double_type_annotation(&mut self, name: &str, span: Span, previous: Span) {
        let message = format!("Type of `{}` is declared twice.", name);
        let previous_message = "Note: previously declared here.";
        self.ctx.reporter
            .symbol_error(message.as_str(), span)
            .span_note(message, span)
            .span_note(previous_message, previous)
            .done();
    }

    fn not_exported(&mut self, item: &str, by: &str, span: Span) {
        let message = format!("Item `{}` is not exported by `{}`.", item, by);
        self.ctx.reporter
            .symbol_error(message.as_str(), span)
            .span_note(message, span)
            .done();
    }

    fn subitem_not_exported(&mut self, item: &str, parent: &str, by: &str, span: Span) {
        let message = format!("Module `{}` does not export `{}` as subitem of `{}`.", by, item, parent);
        self.ctx.reporter
            .symbol_error(message.as_str(), span)
            .span_note(message, span)
            .done();
    }

    fn no_subitems(&mut self, item: &str, by: &str, span: Span) {
        let message = format!("Module `{}` does not export any subitems of `{}`.", by, item);
        self.ctx.reporter
            .symbol_error(message.as_str(), span)
            .span_note(message, span)
            .done();
    }

    fn unknown_symbol(&mut self, kind: Kind, symbol: &Node<p::Symbol>) {
        let message = format!("Unknown {}: `{}`.", kind, symbol.value.clone().full_name());
        self.ctx.reporter
            .symbol_error(message.as_str(), symbol.span)
            .span_note(message, symbol.span)
            .done();
    }

    fn unknown_local_symbol(&mut self, kind: Kind, symbol: &Node<String>) {
        let message = format!("Unknown local {}: `{}`.", kind, symbol.value);
        self.ctx.reporter
            .symbol_error(message.as_str(), symbol.span)
            .span_note(message, symbol.span)
            .done();
    }

    fn unknown_module(&mut self, m: &str, span: Span) {
        let message = format!("Unknown module: `{}`.", m);
        self.ctx.reporter
            .symbol_error(message.as_str(), span)
            .span_note(message, span)
            .done();
    }
    
    fn collect_items<'b>(&mut self, module: &'b Module) -> Exports<'b> {
        let mut values: BTreeMap<&'b str, (Option<&'b str>, Span, Sym)> = BTreeMap::new();
        let mut patterns: BTreeMap<&'b str, (Option<&'b str>, Span, Sym)> = BTreeMap::new();
        let mut traits: BTreeMap<&'b str, (Span, Sym)> = BTreeMap::new();
        let mut types: BTreeMap<&'b str, (Span, Sym)> = BTreeMap::new();

        let module_name = module.name();
        for item in &module.items {
            match item.value {
                Decl::Let(LetDecl::Def(ref def)) => {
                    let vars = def.pattern.value.bound_vars(def.pattern.span);
                    for Node { value: var, span } in vars {
                        match values.entry(var) {
                            Entry::Vacant(entry) => {
                                let sym = self.fresh_value_sym(module_name, var, None);
                                entry.insert((None, span, sym));
                            }
                            Entry::Occupied(entry) => {
                                let another = entry.get().1;
                                // duplicate value, defined at `span` and `another`
                                self.double_definition(
                                    var,
                                    span,
                                    another,
                                );
                            }
                        }
                    }
                }
                Decl::Trait(ref trait_) => {
                    let trait_sym = self.fresh_trait_sym(module_name, &trait_.name.value);
                    let mut info = TraitInfo::new();
                    match traits.entry(&trait_.name.value) {
                        Entry::Vacant(entry) => {
                            entry.insert((trait_.name.span, trait_sym));
                        }
                        Entry::Occupied(entry) => {
                            let span = entry.get().0;
                            // duplicate trait, defined at `trait_.name.span` and `span`
                            self.double_definition(
                                &trait_.name.value,
                                trait_.name.span,
                                span,
                            );
                        }
                    }

                    for value in &trait_.values {
                        let item = &value.value.value.value;
                        match values.entry(item) {
                            Entry::Vacant(entry) => {
                                let sym = self.fresh_value_sym(module_name, item, None);
                                info.item_symbols.insert(item.clone(), sym);
                                entry.insert((None, value.value.value.span, sym));
                            }
                            Entry::Occupied(entry) => {
                                let another = entry.get().1;
                                // duplicate value, defined at `span` and `another`
                                self.double_definition(
                                    item,
                                    value.value.value.span,
                                    another,
                                );
                            }
                        }
                    }

                    self.traits.insert(trait_sym, info);
                }
                Decl::TypeAlias(ref type_) => {
                    match types.entry(&type_.name.value) {
                        Entry::Vacant(entry) => {
                            let sym = self.fresh_type_sym(module_name, &type_.name.value);
                            entry.insert((type_.name.span, sym));
                        }
                        Entry::Occupied(entry) => {
                            let span = entry.get().0;
                            // duplicate type, defined at `type_.name.span` and `span`
                            self.double_definition(
                                &type_.name.value,
                                type_.name.span,
                                span,
                            );
                        }
                    }
                }
                Decl::Union(ref union) => {
                    match types.entry(&union.name.value) {
                        Entry::Vacant(entry) => {
                            let sym = self.fresh_type_sym(module_name, &union.name.value);
                            entry.insert((union.name.span, sym));
                        }
                        Entry::Occupied(entry) => {
                            let span = entry.get().0;
                            // duplicate type, defined at `span` and `union.name.span`
                            self.double_definition(
                                &union.name.value,
                                union.name.span,
                                span,
                            );
                        }
                    }
                    for case in &union.cases {
                        let sym = self.fresh_value_sym(
                            module_name,
                            &case.value.tag.value,
                            Some(&union.name.value));
                        let reported = match values.entry(&case.value.tag.value) {
                            Entry::Vacant(entry) => {
                                entry.insert((
                                    Some(&union.name.value),
                                    case.value.tag.span,
                                    sym,
                                ));
                                false
                            }
                            Entry::Occupied(entry) => {
                                let span = entry.get().1;
                                // duplicate value, defined at `span` and `case.node.tag.span`
                                self.double_definition(
                                    &case.value.tag.value,
                                    case.value.tag.span,
                                    span,
                                );
                                true
                            }
                        };
                        match patterns.entry(&case.value.tag.value) {
                            Entry::Vacant(entry) => {
                                entry.insert((
                                    Some(&union.name.value),
                                    case.value.tag.span,
                                    sym,
                                ));
                            }
                            Entry::Occupied(entry) => {
                                if !reported {
                                    let span = entry.get().1;
                                    // duplicate pattern, defined at `span` and `case.node.tag.span`
                                    self.double_definition(
                                        &case.value.tag.value,
                                        case.value.tag.span,
                                        span,
                                    );
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

        let value_parents = values.iter().filter_map(|(&k, &(p, _, _))| {
            if let Some(parent) = p {
                Some((k, parent))
            } else {
                None
            }
        }).collect();

        let pattern_parents = patterns.iter().filter_map(|(&k, &(p, _, _))| {
            if let Some(parent) = p {
                Some((k, parent))
            } else {
                None
            }
        }).collect();

        let values = values.iter().map(|(&k, &(_, _, s))| (k, s)).collect();
        let patterns = patterns.iter().map(|(&k, &(_, _, s))| (k, s)).collect();
        let types = types.iter().map(|(&k, &(_, s))| (k, s)).collect();
        let traits = traits.iter().map(|(&k, &(_, s))| (k, s)).collect();

        Exports {
            values,
            patterns,
            types,
            traits,
            value_parents,
            pattern_parents,
        }
    }

    fn collect_exports<'b>(&mut self, module: &'b Module, items: &Exports<'b>) -> Exports<'b> {
        let exposed = match module.def {
            ModuleDef::Implicit { .. } => return items.clone(),
            ModuleDef::Explicit { ref exposing, .. } => {
                match exposing.value {
                    ItemList::All => return items.clone(),
                    ItemList::Some(ref items) => items,
                }
            }
        };

        let mut result = Exports::empty();
        let mut export_pos = BTreeMap::new();
        for item in exposed {
            let name = &item.value.name.value;
            let span = item.value.name.span;
            let mut found = false;
            if let Some(sym) = items.get_value_with_parent(name, None) {
                result.values.insert(name, sym);
                match export_pos.entry((name, Kind::Value)) {
                    Entry::Vacant(entry) => {
                        entry.insert(span);
                    }
                    Entry::Occupied(entry) => {
                        self.double_export(
                            Kind::Value,
                            name,
                            span,
                            *entry.get(),
                        );
                    }
                }
                found = true;
            }
            if let Some(sym) = items.get_pattern_with_parent(name, None) {
                result.patterns.insert(name, sym);
                found = true;
            }
            if let Some(sym) = items.get_type(name) {
                result.types.insert(name, sym);
                match export_pos.entry((name, Kind::Type)) {
                    Entry::Vacant(entry) => {
                        entry.insert(span);
                    }
                    Entry::Occupied(entry) => {
                        self.double_export(
                            Kind::Type,
                            name,
                            span,
                            *entry.get(),
                        );
                    }
                }
                found = true;
            }
            if let Some(sym) = items.get_trait(name) {
                result.traits.insert(name, sym);
                match export_pos.entry((name, Kind::Trait)) {
                    Entry::Vacant(entry) => {
                        entry.insert(span);
                    }
                    Entry::Occupied(entry) => {
                        self.double_export(
                            Kind::Trait,
                            name,
                            span,
                            *entry.get(),
                        );
                    }
                }
                found = true;
            }
            if !found {
                // TODO: maybe check if it is instead a subitem of other item,
                // and change error message to be more helpful?
                self.bad_export(
                    format!("`{}` is not defined in this module.", name),
                    span,
                );
                continue;
            }
            match item.value.subitems {
                Some(Node { value: ItemList::All, .. }) => {
                    let mut found_any = false;
                    for (value, parent) in &items.value_parents {
                        if parent == name {
                            let sym = items.get_value(value).unwrap();
                            result.values.insert(value, sym);
                            result.value_parents.insert(value, name);
                            found_any = true;
                        }
                    }
                    for (pattern, parent) in &items.pattern_parents {
                        if parent == name {
                            let sym = items.get_pattern(pattern).unwrap();
                            result.patterns.insert(pattern, sym);
                            result.pattern_parents.insert(pattern, parent);
                            found_any = true;
                        }
                    }
                    if !found_any {
                        self.bad_export(
                            format!("`{}` does not have any subitems.", name),
                            span,
                        );
                    }
                }
                Some(Node { value: ItemList::Some(ref subitems), .. }) => {
                    for item in subitems {
                        let subname = &item.value;
                        let mut is_ok = false;
                        if let Some(sym) = items.get_value_with_parent(subname, Some(name)) {
                            result.values.insert(subname, sym);
                            result.value_parents.insert(subname, name);
                            match export_pos.entry((subname, Kind::Value)) {
                                Entry::Vacant(entry) => {
                                    entry.insert(span);
                                }
                                Entry::Occupied(entry) => {
                                    self.double_export(
                                        Kind::Value,
                                        subname,
                                        item.span,
                                        *entry.get(),
                                    );
                                }
                            }
                            is_ok = true;
                        }
                        if let Some(sym) = items.get_pattern_with_parent(subname, Some(name)) {
                            result.patterns.insert(subname, sym);
                            result.pattern_parents.insert(subname, name);
                            is_ok = true;
                        }
                        if !is_ok {
                            self.bad_export(
                                format!("`{}` does not have subitem `{}`.", name, subname),
                                span,
                            );
                        }
                    }
                }
                None => { }
            }
        }

        result
    }

    fn collect_imports<'b>(&mut self, module: &'b Module, ctx: &Context<'b>) -> Imports<'b> {
        let mut imports = Imports::empty();
        let mut import_span = BTreeMap::new();
        let mut first_import = BTreeMap::new();

        for import in &module.imports {
            let alias: &str = if let Some(ref alias) = import.value.alias {
                &alias.value
            } else {
                &import.value.name.value
            };

            let name = &import.value.name.value;
            match imports.modules.entry(alias) {
                Entry::Vacant(entry) => {
                    entry.insert(name);
                    import_span.insert(alias, import.span);
                }
                Entry::Occupied(_) => {
                    let previous = import_span[alias];
                    self.module_double_import(
                        alias,
                        import.value.name.span,
                        previous,
                    );
                    continue;
                }
            }

            let exports = if let Some(exports) = ctx.exports.get(name) {
                exports
            } else {
                // module is not available, ignore it completely
                continue;
            };

            match import.value.exposing {
                Some(Node { value: ItemList::All, span: list_span }) => {
                    for &type_ in exports.types.keys() {
                        match first_import.entry((type_, Kind::Type)) {
                            Entry::Vacant(entry) => {
                                entry.insert(list_span);
                                imports.types.insert(type_, name);
                            }
                            Entry::Occupied(entry) => {
                                self.double_import(
                                    Kind::Type,
                                    type_,
                                    list_span,
                                    *entry.get(),
                                );
                            }
                        }
                    }
                    for &trait_ in exports.traits.keys() {
                        match first_import.entry((trait_, Kind::Trait)) {
                            Entry::Vacant(entry) => {
                                entry.insert(list_span);
                                imports.traits.insert(trait_, name);
                            }
                            Entry::Occupied(entry) => {
                                self.double_import(
                                    Kind::Trait,
                                    trait_,
                                    list_span,
                                    *entry.get(),
                                );
                            }
                        }
                    }
                    for &pattern in exports.patterns.keys() {
                        match first_import.entry((pattern, Kind::Pattern)) {
                            Entry::Vacant(entry) => {
                                entry.insert(list_span);
                                imports.patterns.insert(pattern, name);
                            }
                            Entry::Occupied(_) => {
                                // don't report error on pattern, because it
                                // will match some error regarding value
                            }
                        }
                    }
                    for &value in exports.values.keys() {
                        match first_import.entry((value, Kind::Value)) {
                            Entry::Vacant(entry) => {
                                entry.insert(list_span);
                                imports.values.insert(value, name);
                            }
                            Entry::Occupied(entry) => {
                                self.double_import(
                                    Kind::Value,
                                    value,
                                    list_span,
                                    *entry.get(),
                                );
                            }
                        }
                    }
                }
                Some(Node { value: ItemList::Some(ref items), .. }) => {
                    for item_ in items {
                        let span = item_.value.name.span;
                        let item = &item_.value.name.value;
                        let mut is_ok = false;
                        if exports.get_type(item).is_some() {
                            match first_import.entry((item, Kind::Type)) {
                                Entry::Vacant(entry) => {
                                    entry.insert(span);
                                    imports.types.insert(item, name);
                                }
                                Entry::Occupied(entry) => {
                                    self.double_import(
                                        Kind::Type,
                                        item,
                                        span,
                                        *entry.get(),
                                    );
                                }
                            }
                            is_ok = true;
                        }
                        if exports.get_trait(item).is_some() {
                            imports.traits.insert(item, name);
                            match first_import.entry((item, Kind::Trait)) {
                                Entry::Vacant(entry) => {
                                    entry.insert(span);
                                    imports.types.insert(item, name);
                                }
                                Entry::Occupied(entry) => {
                                    self.double_import(
                                        Kind::Trait,
                                        item,
                                        span,
                                        *entry.get(),
                                    );
                                }
                            }
                            is_ok = true;
                        }
                        if exports.get_pattern_with_parent(item, None).is_some() {
                            imports.patterns.insert(item, name);
                            match first_import.entry((item, Kind::Pattern)) {
                                Entry::Vacant(entry) => {
                                    entry.insert(span);
                                    imports.types.insert(item, name);
                                }
                                Entry::Occupied(_) => {
                                    // don't report errors on patterns
                                }
                            }
                            is_ok = true;
                        }
                        if exports.get_value_with_parent(item, None).is_some() {
                            imports.values.insert(item, name);
                            match first_import.entry((item, Kind::Value)) {
                                Entry::Vacant(entry) => {
                                    entry.insert(span);
                                    imports.types.insert(item, name);
                                }
                                Entry::Occupied(entry) => {
                                    self.double_import(
                                        Kind::Value,
                                        item,
                                        span,
                                        *entry.get(),
                                    );
                                }
                            }
                            is_ok = true;
                        }
                        if !is_ok {
                            // TODO: maybe check if item is instead a subitem of
                            // other item and change error message to be more
                            // helpful?
                            self.not_exported(item, name, span);
                            continue;
                        }
                        match item_.value.subitems {
                            Some(Node { value: ItemList::All, span: list_span, .. }) => {
                                let mut found_any = false;
                                for (&pattern, &parent) in &exports.pattern_parents {
                                    if parent != item {
                                        continue;
                                    }
                                    found_any = true;
                                    match first_import.entry((pattern, Kind::Pattern)) {
                                        Entry::Vacant(entry) => {
                                            entry.insert(list_span);
                                            imports.patterns.insert(pattern, name);
                                        }
                                        Entry::Occupied(_) => { }
                                    }
                                }
                                for (&value, &parent) in &exports.value_parents {
                                    if parent != item {
                                        continue;
                                    }
                                    found_any = true;
                                    match first_import.entry((value, Kind::Value)) {
                                        Entry::Vacant(entry) => {
                                            entry.insert(list_span);
                                            imports.values.insert(value, name);
                                        }
                                        Entry::Occupied(entry) => {
                                            self.double_import(
                                                Kind::Value,
                                                value,
                                                list_span,
                                                *entry.get(),
                                            );
                                        }
                                    }
                                }
                                if !found_any {
                                    self.no_subitems(item, name, span);
                                }
                            }
                            Some(Node { value: ItemList::Some(ref items), .. }) => {
                                for subitem in items {
                                    let mut is_ok = false;
                                    if exports.get_value_with_parent(&subitem.value, Some(item)).is_some() {
                                        match first_import.entry((&subitem.value, Kind::Value)) {
                                            Entry::Vacant(entry) => {
                                                entry.insert(subitem.span);
                                                imports.values.insert(&subitem.value, name);
                                            }
                                            Entry::Occupied(entry) => {
                                                self.double_import(
                                                    Kind::Value,
                                                    &subitem.value,
                                                    subitem.span,
                                                    *entry.get(),
                                                );
                                            }
                                        }
                                        is_ok = true;
                                    }
                                    if exports.get_pattern_with_parent(&subitem.value, Some(item)).is_some() {
                                        match first_import.entry((&subitem.value, Kind::Pattern)) {
                                            Entry::Vacant(entry) => {
                                                entry.insert(subitem.span);
                                                imports.patterns.insert(&subitem.value, name);
                                            }
                                            Entry::Occupied(_) => { }
                                        }
                                        is_ok = true;
                                    }
                                    if !is_ok {
                                        self.subitem_not_exported(
                                            &subitem.value,
                                            item,
                                            name,
                                            subitem.span,
                                        );
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
        let mut imports = self.collect_imports(module, ctx);
        imports.add_locals(module.name(), ctx.locals);
        let ctx = Context {
            imports: &imports,
            locals: ctx.locals,
            exports: ctx.exports,
            module: ctx.module.clone(),
        };

        let mut fixity_decl = BTreeMap::new();
        let mut type_annotation = BTreeMap::new();
        for decl in &module.items {
            match decl.value {
                Decl::Impl(ref impl_) => {
                    let (impl_, defs) = self.resolve_impl(impl_, &ctx);
                    self.result.impls.push(impl_);
                    self.result.items.extend(defs);
                }
                Decl::Infix(assoc, ref sym, ref prec) => {
                    let sym_ = &sym.value;
                    if imports.values.get(sym_.as_str()) == Some(&ctx.module.as_str()) {
                        match fixity_decl.entry(sym_.as_str()) {
                            Entry::Vacant(entry) => {
                                entry.insert(decl.span);
                                let sym = ctx.locals.get_value(&sym.value).unwrap();
                                self.result.fixities.insert(sym, (assoc, prec.value));
                            }
                            Entry::Occupied(entry) => {
                                let previous = *entry.get();
                                self.double_fixity_decl(
                                    sym_,
                                    sym.span,
                                    previous,
                                );
                            }
                        }
                    } else {
                        let sym = sym.clone().map(p::Symbol::Unqualified);
                        self.unknown_symbol(Kind::LocalValue, &sym);
                    }
                }
                Decl::Let(LetDecl::Def(ref def)) => {
                    let def = self.resolve_def(def, &ctx);
                    self.result.items.extend(def);
                }
                Decl::Let(LetDecl::Type(ref type_)) => {
                    let sym = type_.value.value.as_str();
                    if imports.values.get(sym) == Some(&ctx.module.as_str()) {
                        match type_annotation.entry(sym) {
                            Entry::Vacant(entry) => {
                                entry.insert(type_.value.span);
                                let local = ctx.locals.get_value(sym).unwrap();
                                let sym_node = Node::new(local.clone(), type_.value.span);
                                let rtype = match type_.type_ {
                                    Some(ref t) => self.resolve_scheme(t, &ctx),
                                    None => continue,
                                };
                                let annotation = r::TypeAnnot {
                                    value: sym_node,
                                    type_: rtype,
                                };
                                let annotation = Node::new(annotation, decl.span);
                                self.result.annotations.insert(local, annotation);
                            }
                            Entry::Occupied(entry) => {
                                let previous = *entry.get();
                                self.double_type_annotation(
                                    sym,
                                    type_.value.span,
                                    previous,
                                );
                            }
                        }
                    } else {
                        self.unknown_local_symbol(
                            Kind::Value,
                            &type_.value,
                        );
                    }
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
                    ctx: &Context) -> (r::Impl, Vec<r::Def>) {
        let scheme = self.resolve_scheme(&impl_.scheme, ctx);
        let trait_ = self.resolve_trait_bound(&impl_.trait_, ctx);
        
        let mut defined_symbols = Vec::new();
        for def in &impl_.values {
            def.value.pattern.value.collect_vars(
                &mut defined_symbols,
                def.value.pattern.span);
        }
        let mut locals = Vec::new();
        for sym in defined_symbols {
            locals.push((sym.clone(), self.fresh_sym(sym.value.clone())));
        }
        self.check_dupe_bindings(&locals);
        let symbols = locals
            .iter()
            .cloned()
            .map(|(name, sym)| (name.value, sym))
            .collect::<BTreeMap<_, _>>();

        let trait_items = match trait_.value {
            Symbol::Known(sym) => {
                if let Some(info) = self.traits.get(&sym) {
                    let mut items = BTreeMap::new();
                    for (&name, &sym) in &symbols {
                        if let Some(&s) = info.item_symbols.get(name) {
                            items.insert(sym, s);
                        }
                    }
                    for (name, _) in &info.item_symbols {
                        if !symbols.contains_key(name.as_str()) {
                            // symbol required in trait is missing in impl
                            let message = format!("Missing impl of `{}`.", name);
                            self.ctx.reporter
                                .symbol_error(message.as_str(), impl_.scheme.value.type_.span)
                                .span_note(message, impl_.scheme.value.type_.span)
                                .done();
                        }
                    }
                    items
                } else {
                    BTreeMap::new()
                }
            }
            Symbol::Unknown => BTreeMap::new(),
        };

        // remove locals that are trait items, so that trait items used in
        // definitions would not resolve to this impl, but to the general item
        locals.retain(|&(_, sym)| !trait_items.contains_key(&sym));

        let mut impl_defs = Vec::new();
        let mut other_defs = Vec::new();
        for def in &impl_.values {
            let span = def.span;
            let pat = self.resolve_pattern(&def.value.pattern, &symbols, ctx);
            let value = match def.value.value {
                Some(ref v) => self.resolve_expr_with_locals(v, ctx, &mut locals),
                None => continue,
            };
            let defs = self.resolve_def_raw(pat, value)
                .into_iter()
                .map(|def| Node::new(def, span));
            for def in defs {
                if trait_items.contains_key(&def.value.sym.value) {
                    impl_defs.push(def.value);
                } else {
                    other_defs.push(def.value);
                }
            }
        }

        let impl_ = r::Impl {
            scheme,
            trait_,
            values: impl_defs,
            trait_items,
        };

        (impl_, other_defs)
    }

    fn resolve_def(
                    &mut self,
                    def: &Def,
                    ctx: &Context) -> Vec<r::Def> {
        let vars = def.pattern.value.bound_vars(def.pattern.span);
        let mut var_symbols = BTreeMap::new();
        for var in &vars {
            let sym = ctx.locals.get_value(&var.value).unwrap();
            var_symbols.insert(var.value, sym);
        }
        let pattern = self.resolve_pattern(&def.pattern, &var_symbols, ctx);
        let expr = match def.value.as_ref().map(|e| self.resolve_expr(e, ctx)) {
            Some(expr) => expr,
            None => return Vec::new(),
        };
        self.resolve_def_raw(pattern, expr)
    }

    fn resolve_def_raw(
        &mut self,
        pattern: Node<r::Pattern>,
        expr: Node<r::Expr>,
    ) -> Vec<r::Def> {
        let vars = pattern.value.bound_vars();
        let pattern_span = pattern.span;
        let expr_span = expr.span;
        let def_span = pattern_span.merge(expr_span);
        if vars.len() == 0 {
            // translate
            //     pattern = expr
            // to
            //     fresh = case expr of
            //       pattern -> ()
            let fresh = self.fresh_artificial_sym();
            let tuple = r::Expr::Tuple(Vec::new());
            let tuple = Node::new(tuple, pattern_span);
            let branch = r::CaseBranch {
                pattern: pattern,
                value: tuple,
                guard: None,
            };
            let branch = Node::new(branch, pattern_span);
            let case = r::Expr::Case(Box::new(expr), vec![branch]);
            let case = Node::new(case, def_span);
            let def = r::Def {
                sym: Node::new(fresh, pattern_span),
                value: case,
                artificial: false,
            };
            vec![def]
        } else if vars.len() == 1 {
            // translate
            //     pattern(a) = expr
            // to
            //     a = case expr of
            //       pattern(fresh) -> fresh
            let fresh = self.fresh_artificial_sym();
            let sym = Symbol::Known(fresh);
            let ident = Node::new(r::Expr::Ident(sym), pattern_span);
            let pat = pattern.value.only_with_var(vars[0], fresh);
            let pattern = Node::new(pat, pattern_span);
            let branch = r::CaseBranch {
                pattern: pattern,
                value: ident,
                guard: None,
            };
            let branch = Node::new(branch, pattern_span);
            let case = r::Expr::Case(Box::new(expr), vec![branch]);
            let case = Node::new(case, def_span);
            let def = r::Def {
                sym: Node::new(vars[0], pattern_span),
                value: case,
                artificial: false,
            };
            vec![def]
        } else {
            // translate
            //     pattern(a, b, ...) = expr
            // to
            //     val = expr
            //     a = case val of
            //       pattern(fresh, _, ...) -> fresh
            //     b = case val of
            //       pattern(_, fresh, ...) -> fresh
            //     ...
            let mut defs = Vec::new();
            let val = self.fresh_artificial_sym();
            defs.push(r::Def {
                sym: Node::new(val, pattern_span),
                value: expr,
                artificial: false,
            });
            for var in vars {
                let fresh = self.fresh_artificial_sym();
                let sym = Symbol::Known(fresh);
                let ident = Node::new(r::Expr::Ident(sym), pattern_span);
                let pat = pattern.value.only_with_var(var, fresh);
                let pattern = Node::new(pat, pattern_span);
                let branch = r::CaseBranch {
                    pattern: pattern,
                    value: ident,
                    guard: None,
                };
                let expr = Node::new(r::Expr::Ident(Symbol::Known(val)), pattern_span);
                let branch = Node::new(branch, pattern_span);
                let case = r::Expr::Case(Box::new(expr), vec![branch]);
                let case = Node::new(case, def_span);
                defs.push(r::Def {
                    sym: Node::new(var, pattern_span),
                    value: case,
                    artificial: false,
                });
            }
            defs
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
            let sym = ctx.locals.get_value(&value.value.value.value).unwrap();
            let typ = match value.value.type_ {
                Some(ref t) => self.resolve_scheme(t, ctx),
                None => continue,
            };
            let annot = r::TypeAnnot {
                value: Node::new(sym, value.value.value.span),
                type_: typ,
            };
            values.push(Node::new(annot, value.span));
        }

        let sym = ctx.locals.get_trait(&trait_.name.value).unwrap();
        r::Trait {
            name: Node::new(sym, trait_.name.span),
            base_traits: base_traits,
            values: values,
        }
    }

    fn resolve_type_alias(
                            &mut self,
                            alias: &TypeAlias,
                            ctx: &Context) -> r::TypeAlias {
        let mut vars = Vec::new();
        let mut var_symbols = BTreeMap::new();
        for var in &alias.vars {
            let sym = self.fresh_var_sym(&var.value);
            vars.push(Node::new(sym, var.span));
            var_symbols.insert(var.value.as_str(), sym);
        }
        self.check_dupe_vars(&vars, ctx);
        let resolved_type = alias.type_.as_ref().map(|t| {
            self.resolve_type(t, &mut var_symbols, ctx)
        });

        let sym = ctx.locals.get_type(&alias.name.value).unwrap();
        r::TypeAlias {
            name: Node::new(sym, alias.name.span),
            vars: vars,
            type_: resolved_type,
        }
    }

    fn resolve_union(
                        &mut self,
                        union: &UnionType,
                        ctx: &Context) -> r::UnionType {
        let mut vars = Vec::new();
        let mut var_symbols = BTreeMap::new();
        for var in &union.vars {
            let sym = self.fresh_var_sym(&var.value);
            vars.push(Node::new(sym, var.span));
            var_symbols.insert(var.value.as_str(), sym);
        }
        self.check_dupe_vars(&vars, ctx);
        let mut resolved_cases = Vec::new();
        for case in &union.cases {
            let mut args = Vec::new();
            for arg in &case.value.args {
                args.push(self.resolve_type(arg, &mut var_symbols, ctx));
            }
            let tag_sym = ctx.locals.get_value(&case.value.tag.value).unwrap();
            let resolved_case = r::UnionCase {
                tag: Node::new(tag_sym, case.value.tag.span),
                args: args,
            };
            resolved_cases.push(Node::new(resolved_case, case.span));
        }

        let sym = ctx.locals.get_type(&union.name.value).unwrap();
        r::UnionType {
            name: Node::new(sym, union.name.span),
            vars: vars,
            cases: resolved_cases,
        }
    }

    fn resolve_type<'b>(
        &mut self,
        type_: &'b Node<Type>,
        vars: &BTreeMap<&'b str, Sym>,
        ctx: &Context,
    ) -> Node<r::Type> {
        let resolved = match type_.value {
            Type::Name(ref symbol) => {
                if let p::Symbol::Unqualified(ref name) = *symbol {
                    if let Some(&sym) = vars.get(name.as_str()) {
                        r::Type::Var(sym)
                    } else {
                        let resolved = self.resolve_symbol(
                            &Node::new(symbol.clone(), type_.span),
                            Kind::Type,
                            ctx,
                        );
                        r::Type::Concrete(resolved.value)
                    }
                } else {
                    let resolved = self.resolve_symbol(
                        &Node::new(symbol.clone(), type_.span),
                        Kind::Type,
                        ctx,
                    );
                    r::Type::Concrete(resolved.value)
                }
            }
            Type::Function(ref from, ref to) => {
                let from = self.resolve_type(from, vars, ctx);
                let to = self.resolve_type(to, vars, ctx);
                r::Type::Function(Box::new(from), Box::new(to))
            }
            Type::SelfType => r::Type::SelfType,
            Type::Apply(ref a, ref b) => {
                let a = self.resolve_type(a, vars, ctx);
                let b = self.resolve_type(b, vars, ctx);
                r::Type::Apply(Box::new(a), Box::new(b))
            }
            Type::Tuple(ref items) => {
                r::Type::Tuple(items.iter().map(|t| {
                    self.resolve_type(t, vars, ctx)
                }).collect())
            }
        };

        Node::new(resolved, type_.span)
    }

    fn resolve_scheme(
                    &mut self,
                    type_: &Node<Scheme>,
                    ctx: &Context) -> Node<r::Scheme> {
        let mut prev_def = BTreeMap::new();
        let mut vars = Vec::new();
        let mut resolve_vars = BTreeMap::new();
        for var in &type_.value.vars {
            match prev_def.entry(var.name.value.as_str()) {
                Entry::Vacant(entry) => {
                    entry.insert(var.name.span);
                }
                Entry::Occupied(entry) => {
                    let msg = format!(
                        "var `{}` bound multiple times in scheme",
                        var.name.value,
                    );
                    self.ctx.reporter
                        .symbol_error(msg, var.name.span)
                        .span_note("bound here", var.name.span)
                        .span_note("previously bound here", *entry.get())
                        .done();
                    continue;
                }
            }
            // TODO: warn if same bound is mentioned twice
            let mut bounds = Vec::new();
            for bound in &var.bounds {
                let bound = self.resolve_trait_bound(bound, ctx);
                match bound.value {
                    Symbol::Known(sym) => {
                        bounds.push(Node::new(sym, bound.span));
                    }
                    Symbol::Unknown => {}
                }
            }
            let var_sym = self.fresh_var_sym(&var.name.value);
            vars.push(r::SchemeVar { name: Node::new(var_sym, var.name.span), bounds });
            resolve_vars.insert(var.name.value.as_str(), var_sym);
        }

        let typ = self.resolve_type(&type_.value.type_, &resolve_vars, ctx);

        Node::new(r::Scheme {
            vars,
            type_: typ,
        }, type_.span)
    }

    fn resolve_trait_bound(
                            &mut self,
                            bound: &Node<p::Symbol>,
                            ctx: &Context) -> Node<Symbol> {
        self.resolve_symbol(bound, Kind::Trait, ctx)
    }

    fn resolve_pattern(
                        &mut self,
                        pattern: &Node<Pattern>,
                        var_symbols: &BTreeMap<&str, Sym>,
                        ctx: &Context) -> Node<r::Pattern> {
        let resolved = match pattern.value {
            Pattern::Wildcard => r::Pattern::Wildcard,
            Pattern::Var(ref s) => {
                let sym = Node::new(var_symbols[s.as_str()], pattern.span);
                let wildcard = Node::new(r::Pattern::Wildcard, pattern.span);
                r::Pattern::As(Box::new(wildcard), sym)
            }
            Pattern::Literal(ref lit) => r::Pattern::Literal(lit.clone()),
            Pattern::Deconstruct(ref sym, ref items) => {
                let s = self.resolve_symbol(sym, Kind::Pattern, ctx);
                let items = items.iter().map(|p| {
                    self.resolve_pattern(p, var_symbols, ctx)
                }).collect();
                r::Pattern::Deconstruct(s, items)
            }
            Pattern::Infix(ref lhs, ref sym, ref rhs) => {
                let lhs = self.resolve_pattern(lhs, var_symbols, ctx);
                let rhs = self.resolve_pattern(rhs, var_symbols, ctx);
                let s = self.resolve_symbol(sym, Kind::Pattern, ctx);
                r::Pattern::Infix(Box::new(lhs), s, Box::new(rhs))
            }
            Pattern::As(ref pat, ref alias) => {
                let pat = self.resolve_pattern(pat, var_symbols, ctx);
                let sym = Node::new(var_symbols[alias.value.as_str()], alias.span);
                r::Pattern::As(Box::new(pat), sym)
            }
            Pattern::Parenthesised(ref pat) => {
                let pat = self.resolve_pattern(pat, var_symbols, ctx);
                r::Pattern::Parenthesised(Box::new(pat))
            }
            Pattern::Tuple(ref items) => {
                let items = items.iter().map(|p| {
                    self.resolve_pattern(p, var_symbols, ctx)
                }).collect();
                r::Pattern::Tuple(items)
            }
            Pattern::List(ref items) => {
                let items = items.iter().map(|p| {
                    self.resolve_pattern(p, var_symbols, ctx)
                }).collect();
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

    fn resolve_expr_with_locals<'b>(
                                    &mut self,
                                    expr: &'b Node<Expr>,
                                    ctx: &Context,
                                    locals: &mut Vec<(Node<&'b str>, Sym)>) -> Node<r::Expr> {
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
                    let mut symbols = BTreeMap::new();
                    for var in arm.value.pattern.value.bound_vars(arm.value.pattern.span) {
                        let sym = self.fresh_sym(var.value);
                        symbols.insert(var.value, sym);
                        locals.push((var, sym));
                    }
                    let pat = self.resolve_pattern(&arm.value.pattern, &symbols, ctx);
                    self.check_dupe_bindings(&locals[locals_before..]);
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
                self.resolve_do(do_, ctx, locals)
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
                // translate
                //     \ pattern1(..) pattern2(..) -> value
                // to
                //     \ fresh1 fresh2 -> case fresh1 of
                //         pattern1(..) -> case fresh2 of
                //           pattern2(..) -> value
                let locals_before = locals.len();
                let mut symbols = BTreeMap::new();
                let mut fresh_symbols = Vec::new();
                let mut resolved = Vec::new();
                for param in params {
                    for var in param.value.bound_vars(param.span) {
                        let sym = self.fresh_sym(var.value);
                        symbols.insert(var.value, sym);
                        locals.push((var, sym));
                    }
                    let sym = self.fresh_artificial_sym();
                    fresh_symbols.push(Node::new(sym, param.span));
                    let pattern = self.resolve_pattern(param, &symbols, ctx);
                    resolved.push(pattern);
                }
                let mut result = self.resolve_expr_with_locals(value, ctx, locals);
                for (index, pattern) in resolved.into_iter().enumerate().rev() {
                    let fresh = fresh_symbols[index].value;
                    let pattern_span = pattern.span;
                    let sym = Symbol::Known(fresh);
                    let ident = Node::new(r::Expr::Ident(sym), pattern_span);
                    let span = pattern_span.merge(result.span);
                    let branch = r::CaseBranch {
                        pattern: pattern,
                        value: result,
                        guard: None,
                    };
                    let branch = Node::new(branch, pattern_span);
                    let case = r::Expr::Case(Box::new(ident), vec![branch]);
                    result = Node::new(case, span);
                }
                for sym in fresh_symbols.into_iter().rev() {
                    let span = result.span.merge(sym.span);
                    let lambda = r::Expr::Lambda(sym, Box::new(result));
                    result = Node::new(lambda, span);
                }
                while locals.len() > locals_before {
                    locals.pop();
                }
                result.value
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

    fn resolve_do<'b>(
                        &mut self,
                        expr: &'b Node<DoExpr>,
                        ctx: &Context,
                        locals: &mut Vec<(Node<&'b str>, Sym)>) -> r::Expr {
        match expr.value {
            DoExpr::Done(ref expr) => {
                self.resolve_expr_with_locals(expr, ctx, locals).value
            }
            DoExpr::Bind(ref pat, ref expr, ref rest) => {
                let expr = self.resolve_expr_with_locals(expr, ctx, locals);
                let locals_before = locals.len();
                let mut symbols = BTreeMap::new();
                for var in pat.value.bound_vars(pat.span) {
                    let sym = self.fresh_sym(var.value);
                    symbols.insert(var.value, sym);
                    locals.push((var, sym));
                }
                self.check_dupe_bindings(&locals[locals_before..]);
                let pat = self.resolve_pattern(pat, &symbols, ctx);
                let rest = Node::new(self.resolve_do(rest, ctx, locals), rest.span);
                while locals.len() > locals_before {
                    locals.pop();
                }
                r::Expr::Bind(pat, Box::new(expr), Box::new(rest))
            }
            DoExpr::If(ref cond, ref rest) => {
                let cond = self.resolve_expr_with_locals(cond, ctx, locals);
                let rest = Node::new(self.resolve_do(rest, ctx, locals), rest.span);
                r::Expr::DoIf(Box::new(cond), Box::new(rest))
            }
            DoExpr::Let(ref pat, ref val, ref rest) => {
                let def = Def {
                    pattern: pat.clone(),
                    value: Some(val.clone()),
                };
                let span = pat.span.merge(val.span);
                let decls = vec![Node::new(LetDecl::Def(def), span)];
                let rest = Node::new(Expr::Do(rest.clone()), rest.span);
                let let_ = Expr::Let(decls, Box::new(rest));
                let let_ = Node::new(let_, expr.span);
                // I know that no references derived from 'let_' will escape,
                // because `locals` is restored to previous size after resolving
                // .. right?
                let hack = unsafe { ::std::mem::transmute(&let_) };
                self.resolve_expr_with_locals(hack, ctx, locals).value
            }
            DoExpr::Sequence(ref expr, ref rest) => {
                let expr = self.resolve_expr_with_locals(expr, ctx, locals);
                let rest = Node::new(self.resolve_do(rest, ctx, locals), rest.span);
                let pattern = Node::new(r::Pattern::Wildcard, expr.span);
                r::Expr::Bind(pattern, Box::new(expr), Box::new(rest))
            }
        }
    }

    fn resolve_let<'b>(
                        &mut self,
                        decls: &'b [Node<LetDecl>],
                        value: &'b Node<Expr>,
                        ctx: &Context,
                        locals: &mut Vec<(Node<&'b str>, Sym)>) -> r::Expr {
        let mut defined_symbols = Vec::new();
        let mut resolved_defs = Vec::new();
        let mut annotation_pos = BTreeMap::new();
        for decl in decls {
            if let LetDecl::Def(ref def) = decl.value {
                def.pattern.value.collect_vars(&mut defined_symbols, def.pattern.span);
            }
        }
        let defined_symbols = {
            let mut res = Vec::new();
            for sym in defined_symbols {
                res.push((sym.clone(), self.fresh_sym(sym.value.clone())));
            }
            res
        };
        self.check_dupe_bindings(&defined_symbols);
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
                        );
                    }
                }
                let annotated = defined_symbols
                    .iter()
                    .filter(|s| s.0.value == type_annot.value.value)
                    .next();
                if let Some(&(_, sym)) = annotated {
                    let type_ = match type_annot.type_ {
                        Some(ref t) => self.resolve_scheme(t, ctx),
                        None => continue,
                    };
                    let annot = r::TypeAnnot {
                        value: Node::new(sym, type_annot.value.span),
                        type_: type_,
                    };
                    let annot = Node::new(annot, decl.span);
                    self.result.annotations.insert(sym, annot);
                } else {
                    self.unknown_local_symbol(
                        Kind::Value,
                        &type_annot.value,
                    );
                }
            }
        }
        let locals_before = locals.len();
        let symbols = defined_symbols
            .iter()
            .cloned()
            .map(|(name, sym)| (name.value, sym))
            .collect::<BTreeMap<_, _>>();
        locals.extend(defined_symbols);
        for decl in decls {
            if let LetDecl::Def(ref def) = decl.value {
                let pat = self.resolve_pattern(&def.pattern, &symbols, ctx);
                let value = match def.value {
                    Some(ref v) => self.resolve_expr_with_locals(v, ctx, locals),
                    None => continue,
                };
                let defs = self.resolve_def_raw(pat, value);
                resolved_defs.extend(defs);
            }
        }
        let value = self.resolve_expr_with_locals(value, ctx, locals);
        while locals.len() > locals_before {
            locals.pop();
        }
        r::Expr::Let(resolved_defs, Box::new(value))
    }

    fn check_dupe_bindings(&mut self, locals: &[(Node<&str>, Sym)]) {
        for (index, first) in locals.iter().enumerate() {
            for second in locals.iter().skip(index + 1) {
                if first.0.value == second.0.value {
                    self.duplicate_binding(
                        second.0.value,
                        second.0.span,
                        first.0.span,
                    );
                }
            }
        }
    }

    fn check_dupe_vars(
                        &mut self, 
                        vars: &[Node<Sym>],
                        _ctx: &Context) {
        for (index, first) in vars.iter().enumerate() {
            for second in vars.iter().skip(index + 1) {
                let first_name = self.ctx.symbols.symbol_name(first.value);
                let second_name = self.ctx.symbols.symbol_name(second.value);
                if first_name == second_name {
                    let message = format!(
                        "Type variable `{}` is defined twice.", first_name);
                    self.ctx.reporter
                        .symbol_error(message.as_str(), second.span)
                        .span_note(message, second.span)
                        .span_note("Note: previously defined here.", first.span)
                        .done();
                }
            }
        }
    }

    fn resolve_symbol(
                        &mut self,
                        symbol: &Node<p::Symbol>,
                        kind: Kind,
                        ctx: &Context) -> Node<Symbol> {
        let unknown = Node::new(Symbol::Unknown, symbol.span);
        let sym = match symbol.value {
            p::Symbol::Qualified(ref m, ref n) => {
                let m = if let Some(&m) = ctx.imports.modules.get(m.as_str()) {
                    m
                } else {
                    self.unknown_module(m, symbol.span);
                    return unknown;
                };
                let exports = if let Some(exports) = ctx.exports.get(m) {
                    exports
                } else {
                    return unknown;
                };
                match exports.get_symbol(kind, n) {
                    Some(sym) => Symbol::Known(sym),
                    None => {
                        self.not_exported(n, m, symbol.span);
                        Symbol::Unknown
                    }
                }
            }
            p::Symbol::Unqualified(ref s) => {
                if let Some(m) = ctx.imports.get_origin(kind, s) {
                    let s = if m == ctx.module.as_str() {
                        // value comes from current module, look in ctx.locals
                        ctx.locals.get_symbol(kind, s)
                    } else {
                        // value comes from other module, look in ctx.exports[m]
                        ctx.exports[m].get_symbol(kind, s)
                    };
                    // if we got origin then symbol must exist there,
                    // therefore unwrap
                    Symbol::Known(s.unwrap())
                } else {
                    self.unknown_symbol(kind, symbol);
                    Symbol::Unknown
                }
            }
        };
        Node::new(sym, symbol.span)
    }

    fn resolve_symbol_in_expr(
                                &mut self,
                                symbol: &Node<p::Symbol>,
                                ctx: &Context,
                                locals: &[(Node<&str>, Sym)]) -> Node<Symbol> {
        if let p::Symbol::Unqualified(ref name) = symbol.value {
            for &(ref local, sym) in locals.iter().rev() {
                if local.value == name {
                    return Node::new(Symbol::Known(sym), symbol.span);
                }
            }
        }
        self.resolve_symbol(symbol, Kind::Value, ctx)
    }
}
