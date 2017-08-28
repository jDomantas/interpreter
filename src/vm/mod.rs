use std::collections::BTreeMap;
use std::rc::Rc;
use ast::resolved::Sym;


#[derive(Debug, Clone)]
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
    CallFunction(u64),
    Call(usize),
    TailCallFunction(u64),
    TailCall(usize),
    MakeClosure(u64),
    MakeObject(Sym, usize),
    Return,
    Nip(usize, usize),
    Drop(usize),
    Crash(&'static str),
    IntAdd,
    IntSub,
    IntMul,
    IntDiv,
    IntLe,
    IntEq,
    IntGr,
    IntToString,
    FracAdd,
    FracSub,
    FracMul,
    FracDiv,
    FracLe,
    FracEq,
    FracGr,
    FracToString,
    CharToString,
    CharLe,
    CharEq,
    CharGr,
    StrLength,
    StrCharAt,
    StrAppend,
    StrSubstring,
    StrLe,
    StrEq,
}

#[derive(Debug)]
pub struct Function {
    pub arg_count: usize,
    pub instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub enum GlobalValue {
    Thunk(u64),
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

// TODO: fields should not be pub?
// pub(crate) at worst
#[derive(Debug, Clone)]
pub struct Object {
    pub tag: Sym,
    pub items: Vec<Value>,
}

// TODO: same as above
#[derive(Debug, Clone)]
pub struct Closure {
    pub function: u64,
    pub partial_args: Vec<Value>,
}

#[derive(Debug)]
pub enum EvalError {
    Crashed(&'static str),
}

pub type EvalResult = Result<Value, EvalError>;

#[derive(Debug)]
pub struct Vm<'a> {
    globals: BTreeMap<Sym, GlobalValue>,
    functions: &'a BTreeMap<u64, Function>,
    stack: Vec<Value>,
    frames: Vec<usize>,
    call_stack: Vec<(&'a Function, usize)>,
}

impl<'a> Vm<'a> {
    pub fn new(
            globals: BTreeMap<Sym, GlobalValue>,
            functions: &'a BTreeMap<u64, Function>) -> Vm {
        Vm {
            globals,
            functions,
            stack: Vec::new(),
            frames: Vec::new(),
            call_stack: Vec::new(),
        }
    }

    pub fn eval_globals(&mut self) -> EvalResult {
        self.get_global(::compiler::builtins::values::MAIN)
    }

    fn apply(&mut self, f: Value, args: &[Value]) -> EvalResult {
        for arg in args.iter().rev().cloned() {
            self.stack.push(arg);
        }
        self.stack.push(f);
        self.run_instruction(Instruction::Call(args.len()))?;
        Ok(self.stack.pop().unwrap())
    }

    fn eval_thunk(&mut self, f: u64) -> EvalResult {
        let old_stack_size = self.stack.len();
        let old_call_stack = ::std::mem::replace(&mut self.call_stack, Vec::new());
        let f = &self.functions[&f];
        self.call_stack.push((f, 0));
        while self.call_stack.len() > 0 {
            self.step()?;
        }
        let value = self.stack.pop().unwrap();
        debug_assert_eq!(self.stack.len(), old_stack_size);
        self.call_stack = old_call_stack;
        Ok(value)
    }

    fn step(&mut self) -> Result<(), EvalError> {
        let instruction = match self.call_stack.last_mut() {
            Some(&mut (f, ref mut ip)) => {
                let i = f.instructions[*ip].clone();
                *ip += 1;
                i
            }
            None => {
                return Ok(());
            }
        };
        self.run_instruction(instruction)
    }

    fn get_global(&mut self, sym: Sym) -> EvalResult {
        let function_id = match self.globals[&sym] {
            GlobalValue::Value(ref val) => {
                return Ok(val.clone());
            }
            GlobalValue::Thunk(id) => {
                id
            }
        };
        let value = self.eval_thunk(function_id)?;
        self.globals.insert(sym, GlobalValue::Value(value.clone()));
        Ok(value)
    }

    fn set_ip(&mut self, ip: usize) {
        self.call_stack.last_mut().unwrap().1 = ip;
    }

    fn pop_frame(&mut self) {
        let new_size = self.frames.pop().unwrap();
        self.stack.truncate(new_size);
    }

    fn clear_below(&mut self, to_clear: usize, to_keep: usize) {
        // stack   ...................
        // to_clear         ^~~~~^
        // to_keep                ^~~^
        // remove these     ^~~~~^
        let first_removed = self.stack.len() - (to_clear + to_keep);
        let first_kept = self.stack.len() - to_keep;
        // drain the range, vec will shift top elements afterwards
        for _ in self.stack.drain(first_removed..first_kept) {}
    }

    fn run_instruction(&mut self, i: Instruction) -> Result<(), EvalError> {
        use self::Instruction::*;
        // println!("ins: {:?}, stack: {:?}", i, self.stack);
        match i {
            NoOp => {}
            PushValue(val) => {
                self.stack.push(val);
            }
            PushLocal(depth) => {
                if depth < self.stack.len() {
                    let value = self.stack[self.stack.len() - depth - 1].clone();
                    self.stack.push(value);
                } else {
                    panic!("picking local from too deep");
                }
            }
            PushGlobal(sym) => {
                let value = self.get_global(sym)?;
                self.stack.push(value);
            }
            CreateFrame => {
                self.frames.push(self.stack.len());
            }
            DeleteFrame => {
                self.frames.pop().expect("no frame to be deleted");
            }
            TagTest(tag, fail_jump) => {
                let value = self.stack.last().unwrap().clone();
                match value {
                    Value::Object(ref obj) if obj.tag == tag => {
                        for item in &obj.items {
                            self.stack.push(item.clone());
                        }
                    }
                    _ => {
                        self.pop_frame();
                        self.set_ip(fail_jump);
                    }
                }
            }
            TestBool(val, fail_jump) => {
                let value = self.stack.last().unwrap().clone();
                match value {
                    Value::Bool(b) if b == val => {}
                    Value::Bool(_) => {
                        self.pop_frame();
                        self.set_ip(fail_jump);
                    }
                    other => {
                        panic!("testing bool, but stack contains: {:?}", other);
                    }
                }
            }
            TestInt(val, fail_jump) => {
                let value = self.stack.last().unwrap().clone();
                match value {
                    Value::Int(i) if i == val => {}
                    Value::Int(_) => {
                        self.pop_frame();
                        self.set_ip(fail_jump);
                    }
                    other => {
                        panic!("testing int, but stack contains: {:?}", other);
                    }
                }
            }
            TestFrac(val, fail_jump) => {
                let value = self.stack.last().unwrap().clone();
                match value {
                    Value::Frac(f) if f == val => {}
                    Value::Frac(_) => {
                        self.pop_frame();
                        self.set_ip(fail_jump);
                    }
                    other => {
                        panic!("testing frac, but stack contains: {:?}", other);
                    }
                }
            }
            TestChar(val, fail_jump) => {
                let value = self.stack.last().unwrap().clone();
                match value {
                    Value::Char(c) if c == val => {}
                    Value::Char(_) => {
                        self.pop_frame();
                        self.set_ip(fail_jump);
                    }
                    other => {
                        panic!("testing char, but stack contains: {:?}", other);
                    }
                }
            }
            TestString(val, fail_jump) => {
                let value = self.stack.last().unwrap().clone();
                match value {
                    Value::Str(ref s) if s == &val => {}
                    Value::Str(_) => {
                        self.pop_frame();
                        self.set_ip(fail_jump);
                    }
                    other => {
                        panic!("testing string, but stack contains: {:?}", other);
                    }
                }
            }
            Jump(address) => {
                self.set_ip(address);
            }
            CallFunction(id) => {
                let f = &self.functions[&id];
                self.call_stack.push((f, 0));
            }
            Call(mut arg_count) => {
                while arg_count > 0 {
                    let mut closure = self.pop_closure();
                    let f = &self.functions[&closure.function];
                    let args_missing = f.arg_count - closure.partial_args.len();
                    debug_assert!(args_missing > 0);
                    if arg_count < args_missing {
                        {
                            let args = &mut Rc::make_mut(&mut closure).partial_args;
                            for _ in 0..arg_count {
                                args.push(self.stack.pop().unwrap());
                            }
                        }
                        self.stack.push(Value::Closure(closure));
                        break;
                    } else {
                        arg_count += closure.partial_args.len();
                        arg_count -= f.arg_count;
                        for arg in closure.partial_args.iter().rev().cloned() {
                            self.stack.push(arg);
                        }
                        let cur_stack_size = self.call_stack.len();
                        self.call_stack.push((f, 0));
                        while self.call_stack.len() > cur_stack_size {
                            self.step()?;
                        }
                    }
                }
            }
            TailCallFunction(id) => {
                let f = &self.functions[&id];
                self.call_stack.pop().expect("missing current call frame");
                self.call_stack.push((f, 0));
            }
            TailCall(_arg_count) => {
                unimplemented!()
            }
            /*Apply(mut arg_count) => {
                // self.pop_frames_below(pop_frames, arg_count);
                while arg_count > 0 {
                    let closure = self.pop_closure();
                    let f = &self.functions[&closure.function];
                    let args_missing = f.arg_count as usize - closure.partial_args.len();
                    debug_assert!(args_missing > 0);
                    if arg_count < args_missing {
                        let mut closure = closure;
                        {
                            let args = &mut Rc::make_mut(&mut closure).partial_args;
                            for _ in 0..arg_count {
                                args.push(self.stack.pop().unwrap());
                            }
                        }
                        self.stack.push(Value::Closure(closure));
                        break;
                    } else {
                        for arg in closure.partial_args.iter().rev().cloned() {
                            self.stack.push(arg);
                        }
                        self.call_stack.push((f, 0));
                        arg_count -= args_missing;
                        let cur_size = self.call_stack.len();
                        while self.call_stack.len() >= cur_size {
                            self.step()?;
                        }
                    }
                }
            }*/
            MakeClosure(id) => {
                let closure = Closure {
                    function: id,
                    partial_args: Vec::new(),
                };
                debug_assert!(self.functions[&id].arg_count > 0);
                self.stack.push(Value::Closure(Rc::new(closure)));
            }
            MakeObject(tag, arg_count) => {
                let mut items = Vec::new();
                for _ in 0..arg_count {
                    items.push(self.stack.pop().unwrap());
                }
                let obj = Object { tag, items };
                self.stack.push(Value::Object(Rc::new(obj)));
            }
            Return => {
                self.call_stack.pop().expect("no function to return from");
            }
            Nip(amount, to_keep) => {
                debug_assert!(0 < amount && amount + to_keep <= self.stack.len());
                self.clear_below(amount, to_keep);
            }
            Drop(amount) => {
                debug_assert!(amount > 0);
                let new_size = self.stack.len() - amount;
                self.stack.truncate(new_size);
            }
            Crash(msg) => {
                return Err(EvalError::Crashed(msg));
            }
            IntAdd => {
                let a = self.pop_int();
                let b = self.pop_int();
                self.stack.push(Value::Int(a + b));
            }
            IntSub => {
                let a = self.pop_int();
                let b = self.pop_int();
                self.stack.push(Value::Int(a - b));
            }
            IntMul => {
                let a = self.pop_int();
                let b = self.pop_int();
                self.stack.push(Value::Int(a * b));
            }
            IntDiv => {
                let a = self.pop_int();
                let b = self.pop_int();
                self.stack.push(Value::Int(a / b));
            }
            IntLe => {
                let a = self.pop_int();
                let b = self.pop_int();
                self.stack.push(Value::Bool(a < b));
            }
            IntEq => {
                let a = self.pop_int();
                let b = self.pop_int();
                self.stack.push(Value::Bool(a == b));
            }
            IntGr => {
                let a = self.pop_int();
                let b = self.pop_int();
                self.stack.push(Value::Bool(a > b));
            }
            IntToString => {
                let a = self.pop_int();
                self.stack.push(Value::Str(Rc::new(a.to_string())));
            }
            FracAdd => {
                let a = self.pop_frac();
                let b = self.pop_frac();
                self.stack.push(Value::Frac(a + b));
            }
            FracSub => {
                let a = self.pop_frac();
                let b = self.pop_frac();
                self.stack.push(Value::Frac(a - b));
            }
            FracMul => {
                let a = self.pop_frac();
                let b = self.pop_frac();
                self.stack.push(Value::Frac(a * b));
            }
            FracDiv => {
                let a = self.pop_frac();
                let b = self.pop_frac();
                self.stack.push(Value::Frac(a / b));
            }
            FracLe => {
                let a = self.pop_frac();
                let b = self.pop_frac();
                self.stack.push(Value::Bool(a < b));
            }
            FracEq => {
                let a = self.pop_frac();
                let b = self.pop_frac();
                self.stack.push(Value::Bool(a == b));
            }
            FracGr => {
                let a = self.pop_frac();
                let b = self.pop_frac();
                self.stack.push(Value::Bool(a > b));
            }
            FracToString => {
                let a = self.pop_frac();
                self.stack.push(Value::Str(Rc::new(a.to_string())));
            }
            CharToString => {
                let a = self.pop_char();
                self.stack.push(Value::Str(Rc::new(a.to_string())));
            }
            CharLe => {
                let a = self.pop_char();
                let b = self.pop_char();
                self.stack.push(Value::Bool(a < b));
            }
            CharEq => {
                let a = self.pop_char();
                let b = self.pop_char();
                self.stack.push(Value::Bool(a == b));
            }
            CharGr => {
                let a = self.pop_char();
                let b = self.pop_char();
                self.stack.push(Value::Bool(a > b));
            }
            StrLength => {
                let s = self.pop_string();
                self.stack.push(Value::Int(s.chars().count() as i64));
            }
            StrCharAt => {
                let s = self.pop_string();
                let index = self.pop_int() as usize;
                match s.chars().nth(index) {
                    Some(c) => self.stack.push(make_some(Value::Char(c))),
                    None => self.stack.push(make_none()),
                }
            }
            StrAppend => {
                let mut a = self.pop_string();
                let b = self.pop_string();
                Rc::make_mut(&mut a).push_str(&b);
                self.stack.push(Value::Str(a));
            }
            StrSubstring => {
                let s = self.pop_string();
                let index = self.pop_int() as usize;
                let length = self.pop_int() as usize;
                let s = s.chars().skip(index).take(length).collect();
                self.stack.push(Value::Str(Rc::new(s)));
            }
            StrLe => {
                let a = self.pop_string();
                let b = self.pop_string();
                self.stack.push(Value::Bool(a < b));
            }
            StrEq => {
                let a = self.pop_string();
                let b = self.pop_string();
                self.stack.push(Value::Bool(a == b));
            }
        }
        Ok(())
    }

    fn pop_int(&mut self) -> i64 {
        match self.stack.pop().unwrap() {
            Value::Int(val) => val,
            val => panic!("expected int, got: {:?}", val),
        }
    }

    fn pop_frac(&mut self) -> f64 {
        match self.stack.pop().unwrap() {
            Value::Frac(val) => val,
            val => panic!("expected frac, got: {:?}", val),
        }
    }

    fn pop_char(&mut self) -> char {
        match self.stack.pop().unwrap() {
            Value::Char(val) => val,
            val => panic!("expected char, got: {:?}", val),
        }
    }

    fn pop_string(&mut self) -> Rc<String> {
        match self.stack.pop().unwrap() {
            Value::Str(val) => val,
            val => panic!("expected string, got: {:?}", val),
        }
    }
    
    fn pop_closure(&mut self) -> Rc<Closure> {
        match self.stack.pop().unwrap() {
            Value::Closure(val) => val,
            val => panic!("expected closure, got: {:?}", val),
        }
    }
}

fn make_some(value: Value) -> Value {
    Value::Object(Rc::new(Object {
        tag: ::compiler::builtins::values::SOME,
        items: vec![value],
    }))
}

fn make_none() -> Value {
    Value::Object(Rc::new(Object {
        tag: ::compiler::builtins::values::NONE,
        items: Vec::new(),
    }))
}
