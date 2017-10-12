use std::collections::{BTreeSet, BTreeMap};
use std::rc::Rc;
use ast::{Literal, Sym};
use ast::monomorphised::{Expr, Pattern, Def, Items};
use vm::Vm;
use vm::internal::{Instruction, Value, Object, Closure, GlobalValue};


#[derive(Debug)]
struct Function {
    arg_count: usize,
    instructions: Vec<Instruction>,
}

struct FunctionCtx {
    function: Function,
    id: usize,
    locals: BTreeMap<Sym, usize>,
    stack_size: usize,
}

impl FunctionCtx {
    fn new(id: usize, locals: &[Sym]) -> FunctionCtx {
        let arg_count = locals.len();
        let locals = locals
            .iter()
            .rev()
            .enumerate()
            .map(|(index, &sym)| (sym, index))
            .collect();
        FunctionCtx {
            function: Function {
                arg_count,
                instructions: Vec::new(),
            },
            id,
            locals,
            stack_size: arg_count,
        }
    }

    fn emit(&mut self, instr: Instruction) {
        self.function.instructions.push(instr);
    }

    fn fix_jumps(&mut self, dummy: usize, real: usize) {
        for instr in &mut self.function.instructions {
            match *instr {
                Instruction::Jump(ref mut addr) |
                Instruction::TagTest(_, ref mut addr) |
                Instruction::TestBool(_, ref mut addr) |
                Instruction::TestChar(_, ref mut addr) |
                Instruction::TestFrac(_, ref mut addr) |
                Instruction::TestInt(_, ref mut addr) |
                Instruction::TestString(_, ref mut addr) => {
                    if *addr == dummy {
                        *addr = real;
                    }
                }
                _ => {}
            }
        }
    }

    fn return_top(&mut self) {
        debug_assert!(self.stack_size > 0);
        let nip = self.stack_size - 1;
        if nip > 0 {
            self.emit(Instruction::Nip(nip, 1));
        }
        self.emit(Instruction::Return);
        self.stack_size = 1;
    }

    fn clear_stack(&mut self) {
        if self.stack_size > 0 {
            let amount = self.stack_size;
            self.emit(Instruction::Drop(amount));
            self.stack_size = 0;
        }
    }
}

struct Compiler {
    functions: BTreeMap<usize, Function>,
    next_function_id: usize,
    next_temp_address: usize,
    globals: BTreeMap<Sym, GlobalValue>,
    string_cache: BTreeSet<Rc<String>>,
    object_cache: BTreeMap<Sym, Rc<Object>>,
    ctor_cache: BTreeMap<(Sym, usize), usize>,
    global_symbols: BTreeSet<Sym>,
}

impl Compiler {
    fn new(global_symbols: BTreeSet<Sym>) -> Compiler {
        Compiler {
            functions: BTreeMap::new(),
            next_function_id: 0,
            next_temp_address: ::std::usize::MAX / 2,
            globals: BTreeMap::new(),
            string_cache: BTreeSet::new(),
            object_cache: BTreeMap::new(),
            ctor_cache: BTreeMap::new(),
            global_symbols,
        }
    }

    fn new_function(&mut self, args: &[Sym]) -> FunctionCtx {
        self.next_function_id += 1;
        FunctionCtx::new(self.next_function_id, args)
    }

    fn add_function(&mut self, mut f: FunctionCtx) {
        debug_assert!(!self.functions.contains_key(&f.id));
        f.emit(Instruction::Crash("function did not end in return"));
        self.functions.insert(f.id, f.function);
    }

    fn get_dummy_address(&mut self) -> usize {
        self.next_temp_address += 1;
        self.next_temp_address
    }

    fn make_string(&mut self, string: String) -> Rc<String> {
        if self.string_cache.contains(&string) {
            self.string_cache.get(&string).unwrap().clone()
        } else {
            let rc = Rc::new(string);
            self.string_cache.insert(rc.clone());
            rc
        }
    }

    fn make_object(&mut self, sym: Sym) -> Rc<Object> {
        self.object_cache
            .entry(sym)
            .or_insert_with(|| {
                Rc::new(Object {
                    tag: sym,
                    items: Vec::new(),
                })
            })
            .clone()
    }

    fn make_ctor(&mut self, sym: Sym, args: usize) -> usize {
        if self.ctor_cache.contains_key(&(sym, args)) {
            self.ctor_cache[&(sym, args)]
        } else {
            let mut f = self.new_function(&[]);
            f.function.arg_count = args;
            f.emit(Instruction::MakeObject(sym, args));
            f.emit(Instruction::Return);
            let id = f.id;
            self.ctor_cache.insert((sym, args), id);
            self.add_function(f);
            id
        }
    }

    fn compile_expr(&mut self, expr: Expr, is_tail: bool, f: &mut FunctionCtx) {
        let stack_size_before = f.stack_size;
        match expr {
            Expr::Apply(func, args) => {
                let arg_count = args.len();
                for arg in args.into_iter().rev() {
                    self.compile_expr(arg, false, f);
                }
                self.compile_expr(*func, false, f);
                if is_tail {
                    let nip = f.stack_size - arg_count - 1;
                    if nip > 0 {
                        f.emit(Instruction::Nip(nip, arg_count + 1));
                    }
                    f.emit(Instruction::TailCall(arg_count));
                    f.stack_size = 1;
                } else {
                    f.emit(Instruction::Call(arg_count));
                    f.stack_size -= arg_count;
                }
            }
            Expr::Case(value, branches) => {
                let stack_before = f.stack_size;
                self.compile_expr(*value, false, f);
                debug_assert_eq!(f.stack_size, stack_before + 1);
                let end_address = self.get_dummy_address();
                for branch in branches {
                    f.stack_size = stack_before + 1;
                    f.emit(Instruction::CreateFrame);
                    let fail_address = self.get_dummy_address();
                    self.compile_pattern(branch.pattern, fail_address, f.stack_size - 1, f);
                    if let Some(guard) = branch.guard {
                        self.compile_expr(guard, false, f);
                        f.emit(Instruction::TestBool(true, fail_address));
                    }
                    f.emit(Instruction::DeleteFrame);
                    self.compile_expr(branch.value, is_tail, f);
                    if !is_tail {
                        let nip = f.stack_size - stack_before - 1;
                        if nip > 0 {
                            f.emit(Instruction::Nip(nip, 1));
                            f.stack_size = stack_before + 1;
                        }
                        f.emit(Instruction::Jump(end_address));
                    }
                    let fixed_address = f.function.instructions.len();
                    f.fix_jumps(fail_address, fixed_address);
                }
                f.emit(Instruction::Crash("incomplete match"));
                if !is_tail {
                    let fixed_address = f.function.instructions.len();
                    f.fix_jumps(end_address, fixed_address);
                }
            }
            Expr::Constructor(sym, args) => {
                if is_tail {
                    f.clear_stack();
                }
                if args == 0 {
                    let obj = Value::Object(self.make_object(sym));
                    f.emit(Instruction::PushValue(obj));
                } else {
                    let ctor_id = self.make_ctor(sym, args);
                    f.emit(Instruction::MakeClosure(ctor_id, args));
                }
                f.stack_size += 1;
                if is_tail {
                    f.return_top();
                }
            }
            Expr::Lambda(params, value) => {
                let mut lambda = self.new_function(&params);
                f.emit(Instruction::MakeClosure(lambda.id, lambda.function.arg_count));
                f.stack_size += 1;
                self.compile_expr(*value, true, &mut lambda);
                self.add_function(lambda);
                if is_tail {
                    f.return_top();
                }
            }
            Expr::Let(defs, value) => {
                let def_count = defs.len();
                for def in defs {
                    f.locals.insert(def.sym, f.stack_size);
                    self.compile_expr(def.value, false, f);
                }
                self.compile_expr(*value, is_tail, f);
                if !is_tail {
                    f.emit(Instruction::Nip(def_count, 1));
                }
            }
            Expr::Literal(lit) => {
                if is_tail {
                    f.clear_stack();
                }
                let value = match lit {
                    Literal::Bool(b) => Value::Bool(b),
                    Literal::Char(c) => Value::Char(c),
                    Literal::Float(f) => Value::Frac(f),
                    // TODO: this is very very bad
                    Literal::Int(i) => Value::Int(i as i64),
                    Literal::Str(s) => Value::Str(self.make_string(s)),
                };
                f.emit(Instruction::PushValue(value));
                f.stack_size += 1;
                if is_tail {
                    f.return_top();
                }
            }
            Expr::Var(var) => {
                if self.global_symbols.contains(&var) {
                    if is_tail {
                        f.clear_stack();
                        f.emit(Instruction::PushGlobal(var));
                        f.stack_size += 1;
                        f.return_top();
                    } else {
                        f.emit(Instruction::PushGlobal(var));
                        f.stack_size += 1;
                    }
                } else {
                    let index = f.locals[&var];
                    let depth = f.stack_size - index - 1;
                    f.emit(Instruction::PushLocal(depth));
                    f.stack_size += 1;
                    if is_tail {
                        f.return_top();
                    }
                }
            }
        }
        if is_tail {
            // expressions that return leave a single value on the stack
            debug_assert_eq!(f.stack_size, 1);
        } else {
            // expressions that don't return evaluate to a single value
            debug_assert_eq!(f.stack_size, stack_size_before + 1);
        }
    }

    fn compile_pattern(&mut self, pat: Pattern, fail: usize, val_at: usize, f: &mut FunctionCtx) {
        match pat {
            Pattern::As(pat, sym) => {
                f.locals.insert(sym, val_at);
                self.compile_pattern(*pat, fail, val_at, f);
            }
            Pattern::Deconstruct(sym, parts) => {
                self.extract_val(val_at, f);
                let current_stack = f.stack_size;
                f.emit(Instruction::TagTest(sym, fail));
                f.stack_size += parts.len();
                for (index, part) in parts.into_iter().enumerate().rev() {
                    self.compile_pattern(part, fail, current_stack + index, f);
                }
            }
            Pattern::Literal(Literal::Bool(b)) => {
                self.extract_val(val_at, f);
                f.emit(Instruction::TestBool(b, fail));
            }
            Pattern::Literal(Literal::Char(c)) => {
                self.extract_val(val_at, f);
                f.emit(Instruction::TestChar(c, fail));
            }
            Pattern::Literal(Literal::Float(fr)) => {
                self.extract_val(val_at, f);
                f.emit(Instruction::TestFrac(fr, fail));
            }
            Pattern::Literal(Literal::Int(i)) => {
                self.extract_val(val_at, f);
                // TODO: this very very is wrong
                f.emit(Instruction::TestInt(i as i64, fail));
            }
            Pattern::Literal(Literal::Str(s)) => {
                let s = self.make_string(s);
                self.extract_val(val_at, f);
                f.emit(Instruction::TestString(s, fail));
            }
            Pattern::Wildcard => {}
        }
    }

    fn extract_val(&mut self, val_at: usize, f: &mut FunctionCtx) {
        // if equal, value is already at the top of the stack
        debug_assert!(val_at < f.stack_size);
        if val_at + 1 != f.stack_size {
            let local_at = f.stack_size - 1 - val_at;
            f.emit(Instruction::PushLocal(local_at));
            f.stack_size += 1;
        }
    }

    fn compile_def(&mut self, def: Def) {
        match def.value {
            Expr::Constructor(sym, 0) => {
                let object = Rc::new(Object {
                    tag: sym,
                    items: Vec::new(),
                });
                self.globals.insert(def.sym, GlobalValue::Value(Value::Object(object)));
            }
            Expr::Lambda(params, value) => {
                let mut lambda = self.new_function(&params);
                self.compile_expr(*value, true, &mut lambda);
                let closure = Value::Closure(Rc::new(Closure {
                    address: lambda.id,
                    arg_count: lambda.function.arg_count,
                    partial_args: Vec::new(),
                }));
                self.globals.insert(def.sym, GlobalValue::Value(closure));
                self.add_function(lambda);
            }
            e => {
                let mut f = self.new_function(&[]);
                self.compile_expr(e, true, &mut f);
                self.globals.insert(def.sym, GlobalValue::Thunk(f.id));
                self.add_function(f);
            }
        }
    }

    fn make_builtins(&mut self) {
        use compiler::builtins::values;
        self.make_builtin(values::INT_ADD, 2, Instruction::IntAdd);
        self.make_builtin(values::INT_SUB, 2, Instruction::IntSub);
        self.make_builtin(values::INT_MUL, 2, Instruction::IntMul);
        self.make_builtin(values::INT_DIV, 2, Instruction::IntDiv);
        self.make_builtin(values::INT_EQ, 2, Instruction::IntEq);
        self.make_builtin(values::INT_CMP, 2, Instruction::IntCmp);
        self.make_builtin(values::INT_TO_STR, 1, Instruction::IntToString);
        self.make_builtin(values::FRAC_ADD, 2, Instruction::FracAdd);
        self.make_builtin(values::FRAC_SUB, 2, Instruction::FracSub);
        self.make_builtin(values::FRAC_MUL, 2, Instruction::FracMul);
        self.make_builtin(values::FRAC_DIV, 2, Instruction::FracDiv);
        self.make_builtin(values::FRAC_EQ, 2, Instruction::FracEq);
        self.make_builtin(values::FRAC_CMP, 2, Instruction::FracCmp);
        self.make_builtin(values::FRAC_TO_STR, 1, Instruction::FracToString);
        self.make_builtin(values::CHAR_EQ, 2, Instruction::CharEq);
        self.make_builtin(values::CHAR_CMP, 2, Instruction::CharCmp);
        self.make_builtin(values::CHAR_TO_STR, 1, Instruction::CharToString);
        self.make_builtin(values::STR_APPEND, 2, Instruction::StrAppend);
        self.make_builtin(values::STR_CHAR_AT, 2, Instruction::StrCharAt);
        self.make_builtin(values::STR_LENGTH, 1, Instruction::StrLength);
        self.make_builtin(values::STR_SUBSTRING, 3, Instruction::StrSubstring);
        self.make_builtin(values::STR_CMP, 2, Instruction::StrCmp);
    }

    fn make_builtin(&mut self, sym: Sym, arg_count: usize, instr: Instruction) {
        if !self.globals.contains_key(&sym) {
            return;
        }
        let mut f = self.new_function(&[]);
        f.function.arg_count = arg_count;
        f.emit(instr);
        f.emit(Instruction::Return);
        self.globals.insert(sym, GlobalValue::Value(Value::Closure(Rc::new(Closure {
            address: f.id,
            arg_count,
            partial_args: Vec::new(),
        }))));
        self.add_function(f);
    }
}

fn append_function(instrs: &mut Vec<Instruction>, f: Function) {
    let offset = instrs.len();
    for instr in f.instructions {
        match instr {
            Instruction::Jump(address) => {
                instrs.push(Instruction::Jump(address + offset));
            }
            Instruction::TagTest(tag, address) => {
                instrs.push(Instruction::TagTest(tag, address + offset));
            }
            Instruction::TestBool(value, address) => {
                instrs.push(Instruction::TestBool(value, address + offset));
            }
            Instruction::TestInt(value, address) => {
                instrs.push(Instruction::TestInt(value, address + offset));
            }
            Instruction::TestFrac(value, address) => {
                instrs.push(Instruction::TestFrac(value, address + offset));
            }
            Instruction::TestChar(value, address) => {
                instrs.push(Instruction::TestChar(value, address + offset));
            }
            Instruction::TestString(value, address) => {
                instrs.push(Instruction::TestString(value, address + offset));
            }
            i => {
                instrs.push(i);
            }
        }
    }
}

fn concat_functions(functions: BTreeMap<usize, Function>)
    -> (Vec<Instruction>, BTreeMap<usize, usize>)
{
    let mut result = Vec::new();
    let mut address_mapping = BTreeMap::new();
    for (id, f) in functions {
        address_mapping.insert(id, result.len());
        append_function(&mut result, f);
    }
    for instr in &mut result {
        match *instr {
            Instruction::MakeClosure(ref mut id, _) |
            Instruction::CallFunction(ref mut id) |
            Instruction::TailCallFunction(ref mut id) => {
                *id = address_mapping[id];
            }
            Instruction::PushValue(ref mut value) => {
                if let Value::Closure(ref mut c) = *value {
                    Rc::make_mut(c).address = address_mapping[&c.address];
                }
            }
            _ => {}
        }
    }

    (result, address_mapping)
}

pub fn compile(items: Items) -> Vm {
    let globals = items.items.iter().map(|d| d.sym).collect();
    let mut compiler = Compiler::new(globals);
    for def in items.items {
        compiler.compile_def(def);
    }

    // override dummy builtins with actual builtins
    compiler.make_builtins();

    let (instrs, mapping) = concat_functions(compiler.functions);
    for (_, val) in &mut compiler.globals {
        match *val {
            GlobalValue::Thunk(ref mut id) => {
                *id = mapping[id];
            }
            GlobalValue::Value(ref mut value) => {
                if let Value::Closure(ref mut c) = *value {
                    Rc::make_mut(c).address = mapping[&c.address];
                }
            }
        }
    }
    
    Vm::new(compiler.globals, instrs)
}
