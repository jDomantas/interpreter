pub(crate) mod internal;

use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::rc::Rc;
use ast::Sym;
use self::internal::{Instruction, GlobalValue, Closure, Object, Value as RawValue};


#[derive(Debug, Clone)]
pub enum Value {
    Bool(bool),
    Int(i64),
    Frac(f64),
    Char(char),
    Str(Rc<String>),
    Object(InternalObject),
}

#[derive(Debug, Clone)]
pub struct InternalObject(RawValue);

impl From<RawValue> for Value {
    fn from(value: RawValue) -> Value {
        match value {
            RawValue::Bool(b) => Value::Bool(b),
            RawValue::Char(c) => Value::Char(c),
            RawValue::Frac(f) => Value::Frac(f),
            RawValue::Int(i) => Value::Int(i),
            RawValue::Str(s) => Value::Str(s),
            other => Value::Object(InternalObject(other)),
        }
    }
}

impl From<Value> for RawValue {
    fn from(value: Value) -> RawValue {
        match value {
            Value::Bool(b) => RawValue::Bool(b),
            Value::Char(c) => RawValue::Char(c),
            Value::Frac(f) => RawValue::Frac(f),
            Value::Int(i) => RawValue::Int(i),
            Value::Str(s) => RawValue::Str(s),
            Value::Object(InternalObject(val)) => val,
        }
    }
}

#[derive(Debug)]
pub enum EvalError {
    Crashed(&'static str),
}

pub type EvalResult = Result<Value, EvalError>;

#[derive(Debug)]
pub struct Vm {
    globals: BTreeMap<Sym, GlobalValue>,
    instructions: Vec<Instruction>,
    stack: Vec<RawValue>,
    frames: Vec<usize>,
    call_stack: Vec<usize>,
}

impl Vm {
    pub(crate) fn new(
            globals: BTreeMap<Sym, GlobalValue>,
            instructions: Vec<Instruction>) -> Vm {
        Vm {
            globals,
            instructions,
            stack: Vec::new(),
            frames: Vec::new(),
            call_stack: Vec::new(),
        }
    }

    pub fn get_main(&mut self) -> EvalResult {
        self.get_global(::compiler::builtins::values::MAIN)
    }

    pub fn apply_multiple(&mut self, f: Value, args: &[Value]) -> EvalResult {
        for arg in args.iter().rev().cloned() {
            self.stack.push(arg.into());
        }
        self.stack.push(f.into());
        self.run_instruction(Instruction::Call(args.len()))?;
        Ok(self.stack.pop().unwrap().into())
    }

    pub fn apply(&mut self, f: Value, arg: Value) -> EvalResult {
        let args = &[arg];
        self.apply_multiple(f, args)
    }

    fn eval_thunk(&mut self, f_address: usize) -> EvalResult {
        let old_stack_size = self.stack.len();
        let old_call_stack = ::std::mem::replace(&mut self.call_stack, Vec::new());
        self.call_stack.push(f_address);
        while self.call_stack.len() > 0 {
            self.step()?;
        }
        let value = self.stack.pop().unwrap();
        debug_assert_eq!(self.stack.len(), old_stack_size);
        self.call_stack = old_call_stack;
        Ok(value.into())
    }

    fn step(&mut self) -> Result<(), EvalError> {
        let instruction = match self.call_stack.last_mut() {
            Some(ip) => {
                let i = self.instructions[*ip].clone();
                *ip += 1;
                i
            }
            None => {
                panic!("fell out of function")
            }
        };
        self.run_instruction(instruction)
    }

    fn get_global(&mut self, sym: Sym) -> EvalResult {
        let function_id = match self.globals[&sym] {
            GlobalValue::Value(ref val) => {
                return Ok(val.clone().into());
            }
            GlobalValue::Thunk(id) => {
                id
            }
        };
        let value: RawValue = self.eval_thunk(function_id)?.into();
        self.globals.insert(sym, GlobalValue::Value(value.clone()));
        Ok(value.into())
    }

    fn set_ip(&mut self, ip: usize) {
        *self.call_stack.last_mut().unwrap() = ip;
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
                let value = self.get_global(sym)?.into();
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
                    RawValue::Object(ref obj) if obj.tag == tag => {
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
                    RawValue::Bool(b) if b == val => {}
                    RawValue::Bool(_) => {
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
                    RawValue::Int(i) if i == val => {}
                    RawValue::Int(_) => {
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
                    RawValue::Frac(f) if f == val => {}
                    RawValue::Frac(_) => {
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
                    RawValue::Char(c) if c == val => {}
                    RawValue::Char(_) => {
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
                    RawValue::Str(ref s) if s == &val => {}
                    RawValue::Str(_) => {
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
            CallFunction(address) => {
                self.call_stack.push(address);
            }
            TailCallFunction(address) => {
                self.call_stack.pop().expect("missing current call frame");
                self.call_stack.push(address);
            }
            Call(mut arg_count) => {
                while arg_count > 0 {
                    let mut closure = self.pop_closure();
                    let args_missing = closure.args_missing();
                    debug_assert!(args_missing > 0);
                    if arg_count < args_missing {
                        {
                            let args = &mut Rc::make_mut(&mut closure).partial_args;
                            for _ in 0..arg_count {
                                args.push(self.stack.pop().unwrap());
                            }
                        }
                        self.stack.push(RawValue::Closure(closure));
                        break;
                    } else {
                        arg_count += closure.partial_args.len();
                        arg_count -= closure.arg_count;
                        for arg in closure.partial_args.iter().rev().cloned() {
                            self.stack.push(arg);
                        }
                        let cur_stack_size = self.call_stack.len();
                        self.call_stack.push(closure.address);
                        if arg_count == 0 {
                            break;
                        }
                        while self.call_stack.len() > cur_stack_size {
                            self.step()?;
                        }
                    }
                }
            }
            TailCall(mut arg_count) => {
                while arg_count > 0 {
                    let mut closure = self.pop_closure();
                    let args_missing = closure.args_missing();
                    debug_assert!(args_missing > 0);
                    if arg_count < args_missing {
                        {
                            let args = &mut Rc::make_mut(&mut closure).partial_args;
                            for _ in 0..arg_count {
                                args.push(self.stack.pop().unwrap());
                            }
                        }
                        self.stack.push(RawValue::Closure(closure));
                        self.call_stack.pop();
                        break;
                    } else {
                        arg_count += closure.partial_args.len();
                        arg_count -= closure.arg_count;
                        for arg in closure.partial_args.iter().rev().cloned() {
                            self.stack.push(arg);
                        }
                        let cur_stack_size = self.call_stack.len();
                        if arg_count == 0 {
                            self.call_stack.pop();
                            self.call_stack.push(closure.address);
                            break;
                        }
                        self.call_stack.push(closure.address);
                        while self.call_stack.len() > cur_stack_size {
                            self.step()?;
                        }
                    }
                }
            }
            MakeClosure(address, arg_count) => {
                debug_assert!(arg_count > 0);
                let closure = Closure {
                    address,
                    arg_count,
                    partial_args: Vec::new(),
                };
                self.stack.push(RawValue::Closure(Rc::new(closure)));
            }
            MakeObject(tag, arg_count) => {
                let mut items = Vec::new();
                for _ in 0..arg_count {
                    items.push(self.stack.pop().unwrap());
                }
                let obj = Object { tag, items };
                self.stack.push(RawValue::Object(Rc::new(obj)));
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
                self.stack.push(RawValue::Int(a + b));
            }
            IntSub => {
                let a = self.pop_int();
                let b = self.pop_int();
                self.stack.push(RawValue::Int(a - b));
            }
            IntMul => {
                let a = self.pop_int();
                let b = self.pop_int();
                self.stack.push(RawValue::Int(a * b));
            }
            IntDiv => {
                let a = self.pop_int();
                let b = self.pop_int();
                self.stack.push(RawValue::Int(a / b));
            }
            IntEq => {
                let a = self.pop_int();
                let b = self.pop_int();
                self.stack.push(RawValue::Bool(a == b));
            }
            IntCmp => {
                let a = self.pop_int();
                let b = self.pop_int();
                self.stack.push(make_ordering(a.cmp(&b)));
            }
            IntToString => {
                let a = self.pop_int();
                self.stack.push(RawValue::Str(Rc::new(a.to_string())));
            }
            FracAdd => {
                let a = self.pop_frac();
                let b = self.pop_frac();
                self.stack.push(RawValue::Frac(a + b));
            }
            FracSub => {
                let a = self.pop_frac();
                let b = self.pop_frac();
                self.stack.push(RawValue::Frac(a - b));
            }
            FracMul => {
                let a = self.pop_frac();
                let b = self.pop_frac();
                self.stack.push(RawValue::Frac(a * b));
            }
            FracDiv => {
                let a = self.pop_frac();
                let b = self.pop_frac();
                self.stack.push(RawValue::Frac(a / b));
            }
            FracEq => {
                let a = self.pop_frac();
                let b = self.pop_frac();
                self.stack.push(RawValue::Bool(a == b));
            }
            FracCmp => {
                let a = self.pop_frac();
                let b = self.pop_frac();
                self.stack.push(make_ordering(a.partial_cmp(&b).expect("comparing NaN")));
            }
            FracToString => {
                let a = self.pop_frac();
                self.stack.push(RawValue::Str(Rc::new(a.to_string())));
            }
            CharToString => {
                let a = self.pop_char();
                self.stack.push(RawValue::Str(Rc::new(a.to_string())));
            }
            CharEq => {
                let a = self.pop_char();
                let b = self.pop_char();
                self.stack.push(RawValue::Bool(a == b));
            }
            CharCmp => {
                let a = self.pop_char();
                let b = self.pop_char();
                self.stack.push(make_ordering(a.cmp(&b)));
            }
            StrLength => {
                let s = self.pop_string();
                self.stack.push(RawValue::Int(s.chars().count() as i64));
            }
            StrCharAt => {
                let index = self.pop_int() as usize;
                let s = self.pop_string();
                match s.chars().nth(index) {
                    Some(c) => self.stack.push(make_some(RawValue::Char(c))),
                    None => self.stack.push(make_none()),
                }
            }
            StrAppend => {
                let mut a = self.pop_string();
                let b = self.pop_string();
                Rc::make_mut(&mut a).push_str(&b);
                self.stack.push(RawValue::Str(a));
            }
            StrSubstring => {
                let index = self.pop_int() as usize;
                let length = self.pop_int() as usize;
                let s = self.pop_string();
                let s = s.chars().skip(index).take(length).collect();
                self.stack.push(RawValue::Str(Rc::new(s)));
            }
            StrCmp => {
                let a = self.pop_string();
                let b = self.pop_string();
                self.stack.push(make_ordering(a.cmp(&b)));
            }
        }
        Ok(())
    }

    fn pop_int(&mut self) -> i64 {
        match self.stack.pop().unwrap() {
            RawValue::Int(val) => val,
            val => panic!("expected int, got: {:?}", val),
        }
    }

    fn pop_frac(&mut self) -> f64 {
        match self.stack.pop().unwrap() {
            RawValue::Frac(val) => val,
            val => panic!("expected frac, got: {:?}", val),
        }
    }

    fn pop_char(&mut self) -> char {
        match self.stack.pop().unwrap() {
            RawValue::Char(val) => val,
            val => panic!("expected char, got: {:?}", val),
        }
    }

    fn pop_string(&mut self) -> Rc<String> {
        match self.stack.pop().unwrap() {
            RawValue::Str(val) => val,
            val => panic!("expected string, got: {:?}", val),
        }
    }
    
    fn pop_closure(&mut self) -> Rc<Closure> {
        match self.stack.pop().unwrap() {
            RawValue::Closure(val) => val,
            val => panic!("expected closure, got: {:?}", val),
        }
    }
}

fn make_some(value: RawValue) -> RawValue {
    RawValue::Object(Rc::new(Object {
        tag: ::compiler::builtins::values::SOME,
        items: vec![value],
    }))
}

fn make_none() -> RawValue {
    RawValue::Object(Rc::new(Object {
        tag: ::compiler::builtins::values::NONE,
        items: Vec::new(),
    }))
}

fn make_ordering(o: Ordering) -> RawValue {
    use compiler::builtins::values;
    let tag = match o {
        Ordering::Less => values::LT,
        Ordering::Equal => values::EQ,
        Ordering::Greater => values::GT,
    };
    RawValue::Object(Rc::new(Object {
        tag,
        items: Vec::new(),
    }))
}
