use std::collections::HashMap;
use compiler::builtins::{traits, types, values};


#[derive(PartialEq, PartialOrd, Eq, Ord, Debug, Hash, Copy, Clone)]
pub struct Sym(pub u64);

enum SymbolData {
    Original { name: String, times_derived: u64 },
    Derived { name: String, parent: Sym },
}

pub struct SymbolSource {
    next_sym: u64,
    symbols: HashMap<Sym, SymbolData>,
}

impl SymbolSource {
    pub fn new() -> SymbolSource {
        SymbolSource {
            next_sym: 0,
            symbols: HashMap::new(),
        }
    }

    pub fn fresh_sym(&mut self, name: &str) -> Sym {
        let sym = Sym(self.next_sym);
        self.next_sym += 1;
        self.symbols.insert(sym, SymbolData::Original {
            name: name.into(),
            times_derived: 0,
        });
        sym
    }

    pub fn derived_sym(&mut self, mut from: Sym) -> Sym {
        let (name, parent) = loop {
            match *self.symbols.get_mut(&from).unwrap() {
                SymbolData::Original { ref name, ref mut times_derived } => {
                    *times_derived += 1;
                    let name = format!("{}-{}", name, times_derived);
                    break (name, from);
                }
                SymbolData::Derived { parent, .. } => {
                    from = parent;
                }
            }
        };
        let sym = Sym(self.next_sym);
        self.next_sym += 1;
        self.symbols.insert(sym, SymbolData::Derived { name, parent });
        sym
    }

    pub fn symbol_name(&self, sym: Sym) -> &str {
        match &self.symbols[&sym] {
            &SymbolData::Original { ref name, .. } |
            &SymbolData::Derived { ref name, .. } => {
                name
            }
        }
    }
}

pub const BUILTIN_SYMBOLS: [(Sym, &'static str, &'static str); 48] = [
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
    (values::INT_LE,         "Basics",       "intLe"),
    (values::INT_EQ,         "Basics",       "intEq"),
    (values::INT_GR,         "Basics",       "intGr"),
    (values::INT_TO_STR,     "Basics",       "intToString"),
    (values::FRAC_ADD,       "Basics",       "fracAdd"),
    (values::FRAC_SUB,       "Basics",       "fracSub"),
    (values::FRAC_MUL,       "Basics",       "fracMul"),
    (values::FRAC_DIV,       "Basics",       "fracDiv"),
    (values::FRAC_LE,        "Basics",       "fracLe"),
    (values::FRAC_EQ,        "Basics",       "fracEq"),
    (values::FRAC_GR,        "Basics",       "fracGr"),
    (values::FRAC_TO_STR,    "Basics",       "fracToString"),
    (values::CHAR_TO_STR,    "Basics",       "charToString"),
    (values::CHAR_LE,        "Basics",       "charLe"),
    (values::CHAR_EQ,        "Basics",       "charEq"),
    (values::CHAR_GR,        "Basics",       "charGr"),
    (values::STR_APPEND,     "String",       "append"),
    (values::STR_CHAR_AT,    "String",       "charAt"),
    (values::STR_LENGTH,     "String",       "length"),
    (values::STR_SUBSTRING,  "String",       "substring"),
    (values::STR_LE,         "String",       "less"),
    (values::STR_EQ,         "String",       "equals"),
    (values::MAIN,           "Main",         "main"),
];
