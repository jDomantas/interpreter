use std::rc::Rc;
use ast::Sym;


#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum Instruction {
    NoOp,
    PushValue(Value),
    PushLocal(usize),
    PushGlobal(Sym),
    CreateFrame,
    DeleteFrame,
    TagTest(Sym, usize),
    TestBool(bool, usize),
    TestInt(i64, usize),
    TestFrac(f64, usize),
    TestChar(char, usize),
    TestString(Rc<String>, usize),
    Jump(usize),
    CallFunction(usize),
    Call(usize),
    TailCallFunction(usize),
    TailCall(usize),
    MakeClosure(usize, usize),
    MakeObject(Sym, usize),
    Return,
    Nip(usize, usize),
    Drop(usize),
    Crash(&'static str),
    IntAdd,
    IntSub,
    IntMul,
    IntDiv,
    IntEq,
    IntCmp,
    IntToString,
    FracAdd,
    FracSub,
    FracMul,
    FracDiv,
    FracEq,
    FracCmp,
    FracToString,
    CharToString,
    CharEq,
    CharCmp,
    StrLength,
    StrCharAt,
    StrAppend,
    StrSubstring,
    StrCmp,
}

#[derive(Debug)]
pub enum GlobalValue {
    Thunk(usize),
    Value(Value),
}

#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Int(i64),
    Frac(f64),
    Char(char),
    Str(Rc<String>),
    Object(Rc<Object>),
    Closure(Rc<Closure>),
}

#[derive(Debug, Clone)]
pub struct Object {
    pub tag: Sym,
    pub items: Vec<Value>,
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub address: usize,
    pub arg_count: usize,
    pub partial_args: Vec<Value>,
}

impl Closure {
    pub fn args_missing(&self) -> usize {
        self.arg_count - self.partial_args.len()
    }
}
