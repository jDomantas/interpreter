pub mod types {
    use util::symbols::Sym;
    
    pub const FRAC: Sym = Sym(10);
    pub const BOOL: Sym = Sym(11);
    pub const LIST: Sym = Sym(12);
    pub const CHAR: Sym = Sym(13);
    pub const STRING: Sym = Sym(14);
    pub const INT: Sym = Sym(15);
}

pub mod traits {
    use util::symbols::Sym;
    
    pub const COMPUTATION: Sym = Sym(20);
    pub const DEFAULT: Sym = Sym(21);
    pub const FAILABLE: Sym = Sym(22);
    pub const EQ: Sym = Sym(23);
    pub const ORD: Sym = Sym(24);
    pub const TO_STRING: Sym = Sym(25);
}

pub mod values {
    use util::symbols::Sym;

    pub const NIL: Sym = Sym(40);
    pub const CONS: Sym = Sym(41);

    pub const AND_THEN: Sym = Sym(42);
    pub const DEFAULT: Sym = Sym(43);
    pub const FAIL: Sym = Sym(44);

    pub const AND: Sym = Sym(45);
    pub const OR: Sym = Sym(46);

    pub const MAIN: Sym = Sym(47);

    pub const SOME: Sym = Sym(48);
    pub const NONE: Sym = Sym(49);

    pub const INT_ADD: Sym = Sym(50);
    pub const INT_SUB: Sym = Sym(51);
    pub const INT_MUL: Sym = Sym(52);
    pub const INT_DIV: Sym = Sym(53);
    pub const INT_LE: Sym = Sym(54);
    pub const INT_EQ: Sym = Sym(55);
    pub const INT_GR: Sym = Sym(56);
    pub const INT_TO_STR: Sym = Sym(57);
    
    pub const FRAC_ADD: Sym = Sym(60);
    pub const FRAC_SUB: Sym = Sym(61);
    pub const FRAC_MUL: Sym = Sym(62);
    pub const FRAC_DIV: Sym = Sym(63);
    pub const FRAC_LE: Sym = Sym(64);
    pub const FRAC_EQ: Sym = Sym(65);
    pub const FRAC_GR: Sym = Sym(66);
    pub const FRAC_TO_STR: Sym = Sym(67);

    pub const CHAR_TO_STR: Sym = Sym(70);
    pub const CHAR_LE: Sym = Sym(71);
    pub const CHAR_EQ: Sym = Sym(72);
    pub const CHAR_GR: Sym = Sym(73);

    pub const STR_LENGTH: Sym = Sym(80);
    pub const STR_CHAR_AT: Sym = Sym(81);
    pub const STR_APPEND: Sym = Sym(82);
    pub const STR_SUBSTRING: Sym = Sym(83);
    pub const STR_LE: Sym = Sym(84);
    pub const STR_EQ: Sym = Sym(85);
}

pub mod modules {
    pub const BASICS: &'static str = include_str!("Basics.txt");
    pub const OPTION: &'static str = include_str!("Option.txt");
    pub const LIST: &'static str = include_str!("List.txt");
    pub const STRING: &'static str = include_str!("String.txt");
    pub const COMPUTATION: &'static str = include_str!("Computation.txt");
    pub const RESULT: &'static str = include_str!("Result.txt");
}
