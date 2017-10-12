use std::collections::HashMap;
use compiler::builtins::{traits, types, values};


#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Hash, Copy, Clone)]
pub struct Sym(pub u64);

enum SymbolType {
    Original { name: String, times_derived: u64 },
    Derived { name: String, parent: Sym },
}

pub struct SymbolSource {
    next_sym: u64,
    symbols: HashMap<Sym, SymbolType>,
}

impl SymbolSource {
    pub fn new() -> SymbolSource {
        let mut symbols = HashMap::new();
        for &(sym, _, name) in BUILTIN_SYMBOLS.iter() {
            let data = SymbolType::Original {
                name: name.into(),
                times_derived: 0,
            };
            symbols.insert(sym, data);
        }

        // TODO: fixme
        symbols.insert(Sym(0), SymbolType::Original {
            name: "self".into(),
            times_derived: 0,
        });
        symbols.insert(Sym(1), SymbolType::Original {
            name: "Tuple".into(),
            times_derived: 0,
        });

        SymbolSource {
            next_sym: 100,
            symbols,
        }
    }

    pub fn fresh_sym(&mut self, name: String) -> Sym {
        let sym = Sym(self.next_sym);
        self.next_sym += 1;
        self.symbols.insert(sym, SymbolType::Original {
            name,
            times_derived: 0,
        });
        sym
    }

    pub fn fresh_artificial_sym(&mut self) -> Sym {
        let name = format!("$s{}", self.next_sym);
        self.fresh_sym(name)
    }

    pub fn derived_sym(&mut self, mut from: Sym) -> Sym {
        let (name, parent) = loop {
            match *self.symbols.get_mut(&from).unwrap() {
                SymbolType::Original { ref name, ref mut times_derived } => {
                    *times_derived += 1;
                    let name = format!("{}-{}", name, times_derived);
                    break (name, from);
                }
                SymbolType::Derived { parent, .. } => {
                    from = parent;
                }
            }
        };
        let sym = Sym(self.next_sym);
        self.next_sym += 1;
        self.symbols.insert(sym, SymbolType::Derived { name, parent });
        sym
    }

    pub fn symbol_name(&self, sym: Sym) -> &str {
        match &self.symbols[&sym] {
            &SymbolType::Original { ref name, .. } |
            &SymbolType::Derived { ref name, .. } => {
                name
            }
        }
    }
}

pub const BUILTIN_SYMBOLS: [(Sym, &'static str, &'static str); 44] = [
    (types::INT,             "Basics",       "Int"),
    (types::FRAC,            "Basics",       "Frac"),
    (types::BOOL,            "Basics",       "Bool"),
    (types::CHAR,            "Basics",       "Char"),
    (types::LIST,            "List",         "List"),
    (types::STRING,          "String",       "String"),
    (traits::EQ,             "Basics",       "Eq"),
    (traits::ORD,            "Basics",       "Ord"),
    (traits::DEFAULT,        "Basics",       "Default"),
    (traits::TO_STRING,      "Basics",       "ToString"),
    (traits::COMPUTATION,    "Computation",  "Computation"),
    (traits::FAILABLE,       "Computation",  "Failable"),
    (values::SOME,           "Option",       "Some"),
    (values::NONE,           "Option",       "None"),
    (values::NIL,            "List",         "Nil"),
    (values::CONS,           "List",         "::"),
    (values::AND_THEN,       "Computation",  "andThen"),
    (values::FAIL,           "Computation",  "fail"),
    (values::DEFAULT,        "Basics",       "default"),
    (values::AND,            "Basics",       "&&"),
    (values::OR,             "Basics",       "||"),
    (values::INT_ADD,        "Basics",       "intAdd"),
    (values::INT_SUB,        "Basics",       "intSub"),
    (values::INT_MUL,        "Basics",       "intMul"),
    (values::INT_DIV,        "Basics",       "intDiv"),
    (values::INT_EQ,         "Basics",       "intEq"),
    (values::INT_CMP,        "Basics",       "intCmp"),
    (values::INT_TO_STR,     "Basics",       "intToString"),
    (values::FRAC_ADD,       "Basics",       "fracAdd"),
    (values::FRAC_SUB,       "Basics",       "fracSub"),
    (values::FRAC_MUL,       "Basics",       "fracMul"),
    (values::FRAC_DIV,       "Basics",       "fracDiv"),
    (values::FRAC_EQ,        "Basics",       "fracEq"),
    (values::FRAC_CMP,       "Basics",       "fracCmp"),
    (values::FRAC_TO_STR,    "Basics",       "fracToString"),
    (values::CHAR_TO_STR,    "Basics",       "charToString"),
    (values::CHAR_EQ,        "Basics",       "charEq"),
    (values::CHAR_CMP,       "Basics",       "charCmp"),
    (values::STR_APPEND,     "String",       "append"),
    (values::STR_CHAR_AT,    "String",       "charAt"),
    (values::STR_LENGTH,     "String",       "length"),
    (values::STR_SUBSTRING,  "String",       "substring"),
    (values::STR_CMP,        "String",       "cmp"),
    (values::MAIN,           "Main",         "main"),
];
