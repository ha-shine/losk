mod error;
pub mod native;
pub mod object;
mod types;

use crate::chunk::*;
use crate::object::{Function, NativeFunction};
use crate::value::ConstantValue;
use crate::vm::error::*;
use crate::vm::types::*;
use intrusive_collections::LinkedList;
use native::clock;
use std::collections::HashMap;
use std::fmt;
use std::io::Write;

const DEFAULT_STACK: usize = 256;
const FRAMES_MAX: usize = 256;

pub struct VM<'a> {
    // The stack as a growable vector. In textbook, this is represented by an array and a stack
    // pointer that always point to one element past the top. This is not required here since the
    // vector already give us those functionality.
    stack: Vec<StackValue>,

    // List of objects currently allocated on the heap
    objects: LinkedList<ListAdapter>,

    // The call frame is limited and is allocated on the rust stack for faster access compared to
    // a heap. This comes with limitation that the frame count will be limited and pre-allocation
    // when the VM starts.
    frames: [CallFrame; FRAMES_MAX],
    frame_count: usize,

    // Globals are used to map between a variable defined on the global scope to it's corresponding
    // value. The values are of type `StackValue` because the globals can be mapped to either stack
    // or heap values. The expression after the var declaration statement is pushed onto the stack
    // so it makes sense to map to a `StackValue`. Care should be taken so that the GC scans this
    // table for reachability too.
    globals: HashMap<String, StackValue>,

    // The VM will write the output strings into this stdout
    stdout: &'a mut dyn Write,
}

impl<'a> VM<'a> {
    pub fn new(stdout: &'a mut dyn Write, main: Function) -> Self {
        let main = Object::new(HeapValue::Fun(main));
        let mut objects = LinkedList::new(ListAdapter::new());
        objects.push_front(main.clone());

        // The main function needs to be pushed onto the objects first because doing that will
        // send the object to the heap and the pointer created prior will be invalidated.
        let mut frames: [CallFrame; FRAMES_MAX] = array_init::array_init(|_| CallFrame::default());
        let mut stack = Vec::with_capacity(DEFAULT_STACK);
        stack.push(StackValue::Obj(main.clone()));
        frames[0] = CallFrame {
            fun: main,
            ip: 0,
            slots: 0,
        };

        let clock = Object::new(HeapValue::NativeFunction(NativeFunction::new(
            "clock", 0, clock,
        )));
        objects.push_front(clock.clone());

        let mut globals = HashMap::new();
        globals.insert("clock".to_string(), StackValue::Obj(clock));

        VM {
            stack,
            objects,
            frames,
            frame_count: 1,
            globals,
            stdout,
        }
    }

    pub fn run(&mut self) -> VmResult<()> {
        loop {
            // Instruction is cheap to copy, though the run loop is very sensitive to performance
            // and not sure this would affect the runtime severely.
            let instruction = match self.current_frame_mut().next() {
                Some(instruction) => *instruction,
                None => return Ok(()),
            };

            #[cfg(feature = "debug-trace-execution")]
            for val in &self.stack {
                writeln!(self.stdout, "[ {:10?} ]", val).unwrap();
            }

            match instruction {
                Instruction::Constant(val) => self.execute_constant(val)?,
                Instruction::LiteralTrue => self.push(StackValue::Bool(true)),
                Instruction::LiteralFalse => self.push(StackValue::Bool(false)),
                Instruction::LiteralNil => self.push(StackValue::Nil),
                Instruction::Pop => {
                    self.pop();
                }
                Instruction::PopN(n) => {
                    self.pop_n(n);
                }
                Instruction::GetGlobal(Constant(index)) => {
                    let name = match self
                        .current_frame()
                        .fun()
                        .chunk
                        .get_constant(index as usize)
                    {
                        Some(ConstantValue::Str(name)) => name,
                        _ => panic!("Unreachable"),
                    };

                    match self.globals.get(name) {
                        Some(sval) => self.push(sval.clone()),
                        None => {
                            return Err(self.error(format_args!("Undefined variable '{}'.", name)))
                        }
                    }
                }
                Instruction::DefineGlobal(Constant(index)) => {
                    // The global name is stored as a constant value in the constant pool, read
                    // from the pool to get the name.
                    let name = match self
                        .current_frame()
                        .fun()
                        .chunk
                        .get_constant(index as usize)
                    {
                        Some(ConstantValue::Str(val)) => val.clone(),
                        _ => panic!("Unreachable"),
                    };

                    let val = self.pop();
                    self.globals.insert(name.clone(), val);
                }
                Instruction::SetGlobal(val) => self.execute_set_global(val)?,

                // Additional 1 is added because the stack[0] is the information for current
                // call frame
                Instruction::GetLocal(StackOffset(index)) => {
                    let index = self.current_frame().slots + index as usize + 1;
                    let val = self.stack[index].clone();
                    self.push(val);
                }
                Instruction::SetLocal(StackOffset(index)) => {
                    let index = self.current_frame().slots + index as usize + 1;
                    self.stack[index] = self.stack.last().unwrap().clone();
                }

                Instruction::Equal => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(StackValue::Bool(lhs == rhs));
                }
                Instruction::Greater => self.execute_binary_op(Self::greater)?,
                Instruction::Less => self.execute_binary_op(Self::less)?,
                Instruction::Negate => self.execute_unary_op(Self::neg)?,
                Instruction::Not => self.execute_unary_op(Self::not)?,
                Instruction::Add => self.execute_binary_op(Self::add)?,
                Instruction::Subtract => self.execute_binary_op(Self::sub)?,
                Instruction::Multiply => self.execute_binary_op(Self::mul)?,
                Instruction::Divide => self.execute_binary_op(Self::div)?,
                Instruction::Print => {
                    let val = self.pop();
                    writeln!(self.stdout, "{}", val).unwrap();
                }
                Instruction::JumpIfFalse(JumpDist(dist)) => {
                    // The pointer has already been incremented by 1 before this match statement,
                    // so need to subtract 1 from jump distance.
                    match self.peek() {
                        StackValue::Bool(val) => {
                            if !val {
                                self.current_frame_mut().jump(dist - 1)
                            }
                        }
                        _ => {
                            return Err(
                                self.error(format_args!("Expect test condition to be boolean."))
                            )
                        }
                    }
                }
                Instruction::Jump(JumpDist(dist)) => {
                    // Same ordeal as above here.
                    self.current_frame_mut().jump(dist - 1)
                }
                Instruction::Loop(JumpDist(dist)) => {
                    // Add 1 to distance because the instruction pointer has already been incremented
                    // by 1, so it's pointing to the instruction one after this.
                    self.current_frame_mut().loop_(dist + 1)
                }
                Instruction::Call(ArgCount(count)) => {
                    if self.frame_count == FRAMES_MAX {
                        return Err(self.error(format_args!("Stack overflow.")));
                    }

                    // The previous instructions must have pushed `count` amount to stack, so
                    // the function value would exist at stack[-count].
                    if let StackValue::Obj(obj) = &self.stack[self.stack.len() - count - 1] {
                        match &obj.value {
                            HeapValue::Fun(fun) => {
                                if count != fun.arity {
                                    return Err(self.error(format_args!(
                                        "Expected {} arguments but got {}.",
                                        fun.arity, count
                                    )));
                                }

                                self.frames[self.frame_count] = CallFrame {
                                    fun: obj.clone(),
                                    ip: 0,
                                    slots: self.stack.len() - count - 1,
                                };
                                self.frame_count += 1;
                            }
                            HeapValue::NativeFunction(fun) => {
                                if count != fun.arity {
                                    return Err(self.error(format_args!(
                                        "Expected {} arguments but got {}.",
                                        fun.arity, count
                                    )));
                                }

                                let arg_beg = self.stack.len() - count;
                                let result = (fun.fun)(&self.stack[arg_beg..]);
                                self.pop_n(count + 1); // args + function data
                                if let Some(result) = result {
                                    self.store_value(result)?;
                                }
                            }
                            _ => {
                                return Err(
                                    self.error(format_args!("Can only call functions and classes"))
                                )
                            }
                        }
                    } else {
                        return Err(self.error(format_args!("Can only call functions and classes")));
                    }
                }
                Instruction::Return => {
                    // The return value of a function should be on top of the stack, so it'll be
                    // pushed onto the top of the stack.
                    let result = self.pop();
                    let slots = self.current_frame().slots;
                    self.frame_count -= 1;
                    if self.frame_count == 0 {
                        self.pop();
                        return Ok(());
                    }

                    // After calling a function, the stack top needs to point to the place where
                    // the function information is stored previously (so frame.slots).
                    // To get back there, I need to pop stack.len() - slots.
                    self.pop_n(self.stack.len() - slots);
                    self.push(result);
                }
            }
        }
    }

    fn execute_set_global(&mut self, constant: Constant) -> VmResult<()> {
        // Note that the set variable doesn't pop the value from the stack because the set assignment
        // is an expression statement and there always is a pop instruction after expression statement.
        let val = self.peek().clone();

        // It's a bit wasteful to clone the constant name everytime it's assigned, but I want to
        // avoid unsafe code in this before profiling.
        let name = match self
            .current_frame()
            .fun()
            .chunk
            .get_constant(constant.0 as usize)
        {
            Some(ConstantValue::Str(name)) => name.clone(),
            _ => panic!("Unreachable"),
        };

        match self.globals.get_mut(&name) {
            Some(entry) => {
                *entry = val;
                Ok(())
            }
            None => Err(self.error(format_args!("Undefined variable '{}'.", name))),
        }
    }

    fn store_value(&mut self, value: ConstantValue) -> VmResult<()> {
        match value {
            ConstantValue::Double(val) => self.push(StackValue::Num(val)),
            ConstantValue::Bool(val) => self.push(StackValue::Bool(val)),
            ConstantValue::Str(val) => self.allocate(HeapValue::Str(val)),
            ConstantValue::Fun(fun) => self.allocate(HeapValue::Fun(fun)),
            ConstantValue::Nil => self.push(StackValue::Nil),
        }

        Ok(())
    }

    // The constant value is being cloned here, if the value is a function that means the chunk which
    // can be a large object is being cloned too. This definitely needs to be revisited.
    // An unsafe pointer would work here because the function will always be valid as the main
    // script will always be living at the front of the objects.
    // TODO
    fn execute_constant(&mut self, con: Constant) -> VmResult<()> {
        let constant = self
            .current_frame()
            .fun()
            .chunk
            .get_constant(con.0 as usize)
            .ok_or_else(|| self.error(format_args!("Unknown constant")))?;
        self.store_value(constant.clone())
    }

    fn execute_unary_op(&mut self, op: fn(&mut Self, StackValue) -> OpResult) -> VmResult<()> {
        let val = self.pop();
        let res = op(self, val);
        self.store_op_result(res)
    }

    fn execute_binary_op(
        &mut self,
        op: fn(&mut Self, StackValue, StackValue) -> OpResult,
    ) -> VmResult<()> {
        let rhs = self.pop();
        let lhs = self.pop();
        let res = op(self, lhs, rhs);
        self.store_op_result(res)
    }

    fn store_op_result(&mut self, res: OpResult) -> VmResult<()> {
        match res {
            Ok(StackOrHeap::Stack(val)) => {
                self.stack.push(val);
                Ok(())
            }
            Ok(StackOrHeap::Heap(val)) => {
                self.allocate(val);
                Ok(())
            }
            Err(msg) => Err(self.error(format_args!("{}", msg))),
        }
    }

    // These operators mean that the GC should be carefully considered.
    // Say you're adding two strings, the result string will be allocated on the heap
    // that is the concatenation of the two operands.
    // During the time between the result string has been allocated and before the value (pointer)
    // is saved to stack, that string will not be reachable from the stack. If the GC runs during
    // that time, the result could be gc-ed and the stack value will point to no where.
    // Care needs to be taken when adding values to heap.
    fn add(&mut self, lhs: StackValue, rhs: StackValue) -> OpResult {
        match (lhs, rhs) {
            (StackValue::Num(l), StackValue::Num(r)) => {
                Ok(StackOrHeap::Stack(StackValue::Num(l + r)))
            }
            (StackValue::Obj(l), StackValue::Obj(r)) => match (&l.value, &r.value) {
                (HeapValue::Str(lstr), HeapValue::Str(rstr)) => {
                    let mut res = String::with_capacity(lstr.len() + rstr.len());
                    res += lstr;
                    res += rstr;
                    Ok(StackOrHeap::Heap(HeapValue::Str(res)))
                }
                _ => Err("Expect both operands to be either numbers or strings."),
            },
            (_, _) => Err("Expect both operands to be either numbers or strings."),
        }
    }

    fn sub(&mut self, lhs: StackValue, rhs: StackValue) -> OpResult {
        match (lhs, rhs) {
            (StackValue::Num(l), StackValue::Num(r)) => {
                Ok(StackOrHeap::Stack(StackValue::Num(l - r)))
            }
            (_, _) => Err("Expect both operands to be numbers."),
        }
    }

    fn mul(&mut self, lhs: StackValue, rhs: StackValue) -> OpResult {
        match (lhs, rhs) {
            (StackValue::Num(l), StackValue::Num(r)) => {
                Ok(StackOrHeap::Stack(StackValue::Num(l * r)))
            }
            (_, _) => Err("Expect both operands to be numbers."),
        }
    }

    fn div(&mut self, lhs: StackValue, rhs: StackValue) -> OpResult {
        match (lhs, rhs) {
            (StackValue::Num(l), StackValue::Num(r)) => {
                Ok(StackOrHeap::Stack(StackValue::Num(l / r)))
            }
            (_, _) => Err("Expect both operands to be numbers."),
        }
    }

    fn greater(&mut self, lhs: StackValue, rhs: StackValue) -> OpResult {
        match (lhs, rhs) {
            (StackValue::Num(l), StackValue::Num(r)) => {
                Ok(StackOrHeap::Stack(StackValue::Bool(l > r)))
            }
            (_, _) => Err("Expect both operands to be numbers."),
        }
    }

    fn less(&mut self, lhs: StackValue, rhs: StackValue) -> OpResult {
        match (lhs, rhs) {
            (StackValue::Num(l), StackValue::Num(r)) => {
                Ok(StackOrHeap::Stack(StackValue::Bool(l < r)))
            }
            (_, _) => Err("Expect both operands to be numbers."),
        }
    }

    fn not(&mut self, val: StackValue) -> OpResult {
        match val {
            StackValue::Bool(val) => Ok(StackOrHeap::Stack(StackValue::Bool(!val))),
            _ => Err("Expect operand to be a boolean value."),
        }
    }

    fn neg(&mut self, val: StackValue) -> OpResult {
        match val {
            StackValue::Num(val) => Ok(StackOrHeap::Stack(StackValue::Num(-val))),
            _ => Err("Expect operand to be a number value."),
        }
    }

    fn push(&mut self, val: StackValue) {
        self.stack.push(val)
    }

    fn pop(&mut self) -> StackValue {
        self.stack.pop().unwrap()
    }

    fn pop_n(&mut self, n: usize) {
        let start = self.stack.len() - n;
        self.stack.drain(start..);
    }

    fn peek(&self) -> &StackValue {
        self.stack.last().unwrap()
    }

    fn allocate(&mut self, val: HeapValue) {
        let hval = Object::new(val);
        let sval = StackValue::Obj(hval.clone());

        self.objects.push_front(hval);
        self.stack.push(sval);
    }

    fn current_frame(&self) -> &CallFrame {
        &self.frames[self.frame_count - 1]
    }

    fn current_frame_mut(&mut self) -> &mut CallFrame {
        &mut self.frames[self.frame_count - 1]
    }

    fn error(&self, args: fmt::Arguments) -> Box<RuntimeError> {
        let data: Vec<_> = (0..self.frame_count)
            .rev()
            .map(|idx| {
                let frame = &self.frames[idx];
                let line = *frame.fun().chunk.get_line(frame.ip - 1).unwrap();
                let name = frame.fun().name.clone();
                StackData::new(line, name)
            })
            .collect();

        RuntimeError::new(args, StackTrace(data))
    }
}

// These probably should be integration tests
#[cfg(test)]
mod tests {
    use crate::compiler::Compiler;
    use crate::vm::VM;
    use losk_core::Scanner;
    use std::str;

    fn test_program(src: &str, out: Option<&str>, err: Option<&str>) {
        println!("Testing source:\n{}", src);

        let mut scanner = Scanner::new();
        let compiler = Compiler::new();
        let fun = compiler.compile(scanner.scan_tokens(src)).unwrap();
        let mut output: Vec<u8> = Vec::new();

        let mut vm = VM::new(&mut output, fun);
        let result = vm.run();

        match (result, err) {
            (Err(out), Some(err)) => assert_eq!(err, out.to_string()),
            (Err(out), None) => {
                panic!("Not expecting any error, found '{}'", out)
            }
            (Ok(_), Some(err)) => panic!("Expecting an error '{}', found none.", err),
            _ => {}
        }

        if let Some(out) = out {
            assert_eq!(str::from_utf8(&output).unwrap(), out);
        }

        println!("\n\n");
    }

    #[test]
    fn test_programs() {
        let tests = [
            (
                include_str!("../../../data/print_expression.lox"),
                include_str!("../../../data/print_expression.lox.expected"),
            ),
            (
                include_str!("../../../data/var_assignment.lox"),
                include_str!("../../../data/var_assignment.lox.expected"),
            ),
            (
                include_str!("../../../data/block.lox"),
                include_str!("../../../data/block.lox.expected"),
            ),
            (
                include_str!("../../../data/if_else.lox"),
                include_str!("../../../data/if_else.lox.expected"),
            ),
            (
                include_str!("../../../data/while.lox"),
                include_str!("../../../data/while.lox.expected"),
            ),
            (
                include_str!("../../../data/for.lox"),
                include_str!("../../../data/for.lox.expected"),
            ),
            (
                include_str!("../../../data/print_function.lox"),
                include_str!("../../../data/print_function.lox.expected"),
            ),
            (
                include_str!("../../../data/fun_no_return.lox"),
                include_str!("../../../data/fun_no_return.lox.expected"),
            ),
            (
                include_str!("../../../data/fun_nil_return_print.lox"),
                include_str!("../../../data/fun_nil_return_print.lox.expected"),
            ),
            (
                include_str!("../../../data/fib.lox"),
                include_str!("../../../data/fib.lox.expected"),
            ),
        ];

        for (src, expected) in tests {
            test_program(src, Some(expected), None);
        }
    }

    #[test]
    fn test_stack_trace() {
        let src = include_str!("../../../data/function_stack_trace.lox");
        let exp = include_str!("../../../data/function_stack_trace.lox.expected");

        test_program(src, None, Some(exp));
    }
}
