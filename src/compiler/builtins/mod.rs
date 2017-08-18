pub mod types {
    use ast::resolved::Sym;
    
    pub const FRAC: Sym = Sym(1);
    pub const BOOL: Sym = Sym(2);
    pub const LIST: Sym = Sym(3);
    pub const CHAR: Sym = Sym(4);
    pub const STRING: Sym = Sym(5);
    pub const INT: Sym = Sym(6);
}

pub mod traits {
    use ast::resolved::Sym;
    
    pub const MONAD: Sym = Sym(20);
    pub const DEFAULT: Sym = Sym(21);
    pub const NUMBER: Sym = Sym(22);
    pub const EQ: Sym = Sym(23);
    pub const ORD: Sym = Sym(24);
    pub const TO_STRING: Sym = Sym(25);
}

pub mod values {
    use ast::resolved::Sym;

    pub const NIL: Sym = Sym(40);
    pub const CONS: Sym = Sym(41);

    pub const BIND: Sym = Sym(42);
    pub const DEFAULT: Sym = Sym(43);

    pub const AND: Sym = Sym(44);
    pub const OR: Sym = Sym(45);

    pub const INT_ADD: Sym = Sym(50);
    pub const INT_SUB: Sym = Sym(51);
    pub const INT_MUL: Sym = Sym(52);
    pub const INT_DIV: Sym = Sym(53);
    pub const INT_LE: Sym = Sym(54);
    pub const INT_EQ: Sym = Sym(55);
    pub const INT_GR: Sym = Sym(56);
    
    pub const FRAC_ADD: Sym = Sym(60);
    pub const FRAC_SUB: Sym = Sym(61);
    pub const FRAC_MUL: Sym = Sym(62);
    pub const FRAC_DIV: Sym = Sym(63);
    pub const FRAC_LE: Sym = Sym(64);
    pub const FRAC_EQ: Sym = Sym(65);
    pub const FRAC_GR: Sym = Sym(66);
}

pub mod modules {
    pub const BASICS: &'static str = include_str!("Basics.txt");
    pub const OPTION: &'static str = include_str!("Option.txt");
    pub const LIST: &'static str = include_str!("List.txt");
    pub const STRING: &'static str = include_str!("String.txt");
}
