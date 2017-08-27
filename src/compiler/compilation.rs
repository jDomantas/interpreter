use std::collections::{HashSet, HashMap};
use std::rc::Rc;
use ast::Literal;
use ast::monomorphised::{Sym, Expr, Pattern, Def, Items};
use vm::{Instruction, Function, Value, Object, Closure, GlobalValue};


struct FunctionCtx {
    function: Function,
    id: u64,
    locals: HashMap<Sym, usize>,
    current_size: usize,
    frames: usize,
}

impl FunctionCtx {
    fn new(id: u64, locals: &[Sym]) -> FunctionCtx {
        let arg_count = locals.len() as u64;
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
            current_size: arg_count as usize,
            frames: 0,
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
}

struct Compiler {
    functions: HashMap<u64, Function>,
    next_function: u64,
    next_address: usize,
    globals: HashMap<Sym, GlobalValue>,
    string_cache: HashSet<Rc<String>>,
    global_symbols: HashSet<Sym>,
}

impl Compiler {
    fn new(global_symbols: HashSet<Sym>) -> Compiler {
        Compiler {
            functions: HashMap::new(),
            next_function: 0,
            next_address: 0,
            globals: HashMap::new(),
            string_cache: HashSet::new(),
            global_symbols,
        }
    }

    fn new_function(&mut self, args: &[Sym]) -> FunctionCtx {
        self.next_function += 1;
        FunctionCtx::new(self.next_function, args)
    }

    fn get_dummy_address(&mut self) -> usize {
        self.next_address += 1;
        self.next_address
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

    fn compile_expr(&mut self, expr: Expr, f: &mut FunctionCtx) {
        match expr {
            Expr::Apply(fun, args) => {
                let previous_stack = f.current_size;
                let arg_count = args.len();
                for arg in args.into_iter().rev() {
                    self.compile_expr(arg, f);
                }
                self.compile_expr(*fun, f);
                f.emit(Instruction::Apply(arg_count));
                f.current_size = previous_stack + 1;
            }
            Expr::Case(value, branches) => {
                f.emit(Instruction::CreateFrame);
                f.frames += 1;
                let previous_stack = f.current_size;
                self.compile_expr(*value, f);
                let end_address = self.get_dummy_address();
                f.frames += 1;
                for branch in branches {
                    f.emit(Instruction::CreateFrame);
                    let current_stack = f.current_size;
                    let fail_address = self.get_dummy_address();
                    self.compile_pattern(branch.pattern, fail_address, current_stack - 1, f);
                    if let Some(guard) = branch.guard {
                        self.compile_expr(guard, f);
                        f.emit(Instruction::TestBool(true, fail_address));
                    }
                    self.compile_expr(branch.value, f);
                    f.emit(Instruction::FrameReturn);
                    f.current_size = current_stack;
                    f.emit(Instruction::Jump(end_address));
                    let fixed_address = f.function.instructions.len();
                    f.fix_jumps(fail_address, fixed_address);
                }
                f.emit(Instruction::Crash("incomplete match"));
                let fixed_address = f.function.instructions.len();
                f.fix_jumps(end_address, fixed_address);
                f.emit(Instruction::FrameReturn);
                f.current_size = previous_stack + 1;
                f.frames -= 2;
            }
            Expr::Constructor(sym, args) => {
                let mut ctor = self.new_function(&[]);
                ctor.function.arg_count = args;
                ctor.emit(Instruction::MakeObject(sym, args));
                ctor.emit(Instruction::Return(0));
                let id = ctor.id;
                self.functions.insert(ctor.id, ctor.function);
                f.emit(Instruction::MakeClosure(id));
                f.current_size += 1;
            }
            Expr::Lambda(params, value) => {
                let mut lambda = self.new_function(&params);
                self.compile_expr(*value, &mut lambda);
                let instr = Instruction::Return(lambda.function.arg_count as usize);
                lambda.emit(instr);
                let id = lambda.id;
                self.functions.insert(lambda.id, lambda.function);
                f.emit(Instruction::MakeClosure(id));
                f.current_size += 1;
            }
            Expr::Let(_defs, _value) => {
                unimplemented!()
            }
            Expr::Literal(Literal::Bool(b)) => {
                f.emit(Instruction::PushValue(Value::Bool(b)));
                f.current_size += 1;
            }
            Expr::Literal(Literal::Char(c)) => {
                f.emit(Instruction::PushValue(Value::Char(c)));
                f.current_size += 1;
            }
            Expr::Literal(Literal::Float(fr)) => {
                f.emit(Instruction::PushValue(Value::Frac(fr)));
                f.current_size += 1;
            }
            Expr::Literal(Literal::Int(i)) => {
                // TODO: this is very (not really) wrong
                f.emit(Instruction::PushValue(Value::Int(i as i64)));
                f.current_size += 1;
            }
            Expr::Literal(Literal::Str(s)) => {
                let s = self.make_string(s);
                f.emit(Instruction::PushValue(Value::Str(s)));
                f.current_size += 1;
            }
            Expr::Var(var) => {
                if self.global_symbols.contains(&var) {
                    f.emit(Instruction::PushGlobal(var));
                } else {
                    let index = f.locals[&var];
                    let depth = f.current_size - index - 1;
                    f.emit(Instruction::PushLocal(depth));
                }
                f.current_size += 1;
            }
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
                let current_stack = f.current_size;
                f.emit(Instruction::TagTest(sym, fail));
                f.current_size += parts.len();
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
                // TODO: this is wrong
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
        debug_assert!(val_at < f.current_size);
        if val_at + 1 != f.current_size {
            let local_at = f.current_size - 1 - val_at;
            f.emit(Instruction::PushLocal(local_at));
            f.current_size += 1;
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
            e => {
                let mut f = self.new_function(&[]);
                self.compile_expr(e, &mut f);
                self.globals.insert(def.sym, GlobalValue::Thunk(f.id));
                self.functions.insert(f.id, f.function);
            }
        }
    }

    fn make_builtins(&mut self) {
        use compiler::builtins::values;
        self.make_builtin(values::INT_ADD, 2, Instruction::IntAdd);
        self.make_builtin(values::INT_SUB, 2, Instruction::IntSub);
        self.make_builtin(values::INT_MUL, 2, Instruction::IntMul);
        self.make_builtin(values::INT_DIV, 2, Instruction::IntDiv);
        self.make_builtin(values::INT_LE, 2, Instruction::IntLe);
        self.make_builtin(values::INT_EQ, 2, Instruction::IntEq);
        self.make_builtin(values::INT_GR, 2, Instruction::IntGr);
        self.make_builtin(values::INT_TO_STR, 1, Instruction::IntToString);
        self.make_builtin(values::FRAC_ADD, 2, Instruction::FracAdd);
        self.make_builtin(values::FRAC_SUB, 2, Instruction::FracSub);
        self.make_builtin(values::FRAC_MUL, 2, Instruction::FracMul);
        self.make_builtin(values::FRAC_DIV, 2, Instruction::FracDiv);
        self.make_builtin(values::FRAC_LE, 2, Instruction::FracLe);
        self.make_builtin(values::FRAC_EQ, 2, Instruction::FracEq);
        self.make_builtin(values::FRAC_GR, 2, Instruction::FracGr);
        self.make_builtin(values::FRAC_TO_STR, 1, Instruction::FracToString);
        self.make_builtin(values::CHAR_LE, 2, Instruction::CharLe);
        self.make_builtin(values::CHAR_EQ, 2, Instruction::CharEq);
        self.make_builtin(values::CHAR_GR, 2, Instruction::CharGr);
        self.make_builtin(values::CHAR_TO_STR, 1, Instruction::CharToString);
        self.make_builtin(values::STR_APPEND, 2, Instruction::StrAppend);
        self.make_builtin(values::STR_CHAR_AT, 2, Instruction::StrCharAt);
        self.make_builtin(values::STR_LENGTH, 1, Instruction::StrLength);
        self.make_builtin(values::STR_SUBSTRING, 3, Instruction::StrSubstring);
    }

    fn make_builtin(&mut self, sym: Sym, arg_count: u64, instr: Instruction) {
        let mut f = self.new_function(&[]);
        f.function.arg_count = arg_count;
        f.emit(instr);
        self.globals.insert(sym, GlobalValue::Value(Value::Closure(Rc::new(Closure {
            function: f.id,
            partial_args: Vec::new(),
        }))));
        self.functions.insert(f.id, f.function);
    }
}

pub fn compile(items: Items) -> (HashMap<u64, Function>, HashMap<Sym, GlobalValue>) {
    let globals = items.items.iter().map(|d| d.sym).collect();
    let mut compiler = Compiler::new(globals);
    for def in items.items {
        compiler.compile_def(def);
    }
    // override dummy builtins with actual builtins
    compiler.make_builtins();
    (compiler.functions, compiler.globals)
}
