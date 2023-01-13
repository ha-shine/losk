use crate::chunk::{Chunk, Instruction};
use crate::error::Error;
use crate::instruction::{Constant, JumpDist, StackOffset};
use crate::r#ref::UnsafeRef;
use crate::value::Value;
use crate::VmResult;
use intrusive_collections::{intrusive_adapter, LinkedList, LinkedListLink};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::io::Write;
use std::rc::Rc;

const DEFAULT_STACK: usize = 256;

#[allow(dead_code)]
#[derive(Copy, Clone, PartialEq)]
enum StackValue {
    Num(f64),
    Bool(bool),

    // This is not idiomatic, but for the sake of following the book this is fine for now.
    // Plus doing this as reference means there will be a sea of lifetime indicators in here.
    // This probably should be refactored out into its own RefCell-ish type, but this will suffice
    // for now.
    Obj(UnsafeRef<Object>),
    Nil,
}

impl Display for StackValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StackValue::Num(val) => write!(f, "{}", val),
            StackValue::Bool(val) => write!(f, "{}", val),
            StackValue::Obj(val) => write!(f, "{}", val.borrow().value),
            StackValue::Nil => write!(f, "nil"),
        }
    }
}

impl Debug for StackValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StackValue::Num(val) => write!(f, "{}", val),
            StackValue::Bool(val) => write!(f, "{}", val),
            StackValue::Obj(val) => write!(f, "{:?} -> {}", val, val.borrow().value),
            StackValue::Nil => write!(f, "nil"),
        }
    }
}

#[allow(dead_code)]
#[derive(PartialEq)]
pub(crate) enum HeapValue {
    Str(String),
}

impl Display for HeapValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            HeapValue::Str(val) => write!(f, "{}", val),
        }
    }
}

#[allow(dead_code)]
pub(crate) struct Object {
    link: LinkedListLink,
    value: HeapValue,
}

intrusive_adapter!(ListAdapter = Box<Object>: Object { link: LinkedListLink });

impl Object {
    fn new(val: HeapValue) -> Box<Object> {
        Box::new(Object {
            link: LinkedListLink::new(),
            value: val,
        })
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.value.eq(&other.value)
    }
}

#[allow(dead_code)]
pub(crate) struct VM {
    // instruction pointer, this will be incremented during interpretation
    ip: usize,

    // The stack as a growable vector. In textbook, this is represented by an array and a stack
    // pointer that always point to one element past the top. This is not required here since the
    // vector already give us those functionality.
    stack: Vec<StackValue>,

    // List of objects currently allocated on the heap
    objects: LinkedList<ListAdapter>,

    // Probably would be better to use a context object for compiling, so I dun have to pass
    // the chunk around, or take ownership of it in the VM. But currently, creating a VM entails
    // creating a new stack as the most heavy operation so it's not a big deal yet.
    // As the initialisation task gets heavier, the scale will start tipping more to reusing
    // existing VM and resetting the stack (and ip) after runtime errors.
    // Also it would be cool to support some sort of debugger, where the user can walk the chunk
    // instructions as the VM executes them.
    chunk: Chunk,

    // Globals are used to map between a variable defined on the global scope to it's corresponding
    // value. The values are of type `StackValue` because the globals can be mapped to either stack
    // or heap values. The expression after the var declaration statement is pushed onto the stack
    // so it makes sense to map to a `StackValue`. Care should be taken so that the GC scans this
    // table for reachability too.
    globals: HashMap<String, StackValue>,

    // The VM will write the output strings into this stdout
    stdout: Rc<RefCell<dyn Write>>,
}

enum StackOrHeap {
    Stack(StackValue),
    Heap(HeapValue),
}

type OpResult = Result<StackOrHeap, &'static str>;

#[allow(dead_code)]
impl VM {
    pub(crate) fn new(stdout: Rc<RefCell<dyn Write>>, chunk: Chunk) -> Self {
        VM {
            ip: 0,
            stack: Vec::with_capacity(DEFAULT_STACK),
            objects: LinkedList::new(ListAdapter::new()),
            chunk,
            globals: HashMap::new(),
            stdout,
        }
    }

    pub(crate) fn interpret(&mut self) -> VmResult<()> {
        self.ip = 0;
        Ok(())
    }

    fn run(&mut self) -> VmResult<()> {
        loop {
            let instruction = match self.chunk.get_instruction(self.ip) {
                Some(instruction) => instruction,
                None => return Ok(()),
            };

            #[cfg(feature = "debug-trace-execution")]
            for val in &self.stack {
                writeln!(self.stdout.borrow_mut(), "[ {:10?} ]", val).unwrap();
            }

            // TODO: Disassemble this instruction if DEBUG_TRACE_EXECUTION is defined. But I guess
            //   I don't need it here because the instructions are already disassembled into
            //   `Instruction` object. Maybe the textbook will not do the disassembling ahead of
            //   time and operate on a byte stream?
            self.ip += 1;

            match instruction {
                Instruction::Constant(val) => self.execute_constant(*val)?,
                Instruction::LiteralTrue => self.push(StackValue::Bool(true)),
                Instruction::LiteralFalse => self.push(StackValue::Bool(false)),
                Instruction::LiteralNil => self.push(StackValue::Nil),
                Instruction::Pop => {
                    self.pop();
                }
                Instruction::PopN(n) => {
                    self.pop_n(*n);
                }
                Instruction::GetGlobal(Constant { index }) => {
                    let name = match self.chunk.get_constant(*index as usize) {
                        Some(Value::Str(name)) => name,
                        _ => panic!("Unreachable"),
                    };

                    match self.globals.get(name) {
                        Some(sval) => self.push(*sval),
                        None => {
                            return Err(Error::runtime(
                                *self.chunk.get_line(self.ip - 1).unwrap(),
                                &format!("Undefined variable '{}'.", name),
                            ))
                        }
                    }
                }
                Instruction::DefineGlobal(Constant { index }) => {
                    // The global name is stored as a constant value in the constant pool, read
                    // from the pool to get the name.
                    let name = match self.chunk.get_constant(*index as usize) {
                        Some(Value::Str(val)) => val.clone(),
                        _ => panic!("Unreachable"),
                    };

                    let val = self.pop();
                    self.globals.insert(name.clone(), val);
                }
                Instruction::SetGlobal(val) => self.execute_set_global(*val)?,
                Instruction::GetLocal(StackOffset { index }) => {
                    let val = self.stack[*index as usize];
                    self.push(val);
                }
                Instruction::SetLocal(StackOffset { index }) => {
                    self.stack[*index as usize] = *self.stack.last().unwrap();
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
                    writeln!(self.stdout.borrow_mut(), "{}", val).unwrap();
                }
                Instruction::JumpIfFalse(JumpDist { dist }) => {
                    // The pointer has already been incremented by 1 before this match statement,
                    // so need to subtract 1 from jump distance.
                    match self.peek() {
                        StackValue::Bool(val) => {
                            if !val {
                                self.ip += dist - 1;
                            }
                        }
                        _ => {
                            return Err(Error::runtime(
                                *self.chunk.get_line(self.ip - 1).unwrap(),
                                "Expect test condition to be boolean.",
                            ))
                        }
                    }
                }
                Instruction::Jump(JumpDist { dist }) => {
                    // Same ordeal as above here.
                    self.ip += dist - 1;
                }
                Instruction::Loop(JumpDist { dist }) => {
                    // Add 1 to distance because the instruction pointer has already been incremented
                    // by 1, so it's pointing to the instruction one after this.
                    self.ip -= dist + 1;
                }
                Instruction::Return => {
                    self.pop();
                }
            }
        }
    }

    fn execute_set_global(&mut self, constant: Constant) -> VmResult<()> {
        // Note that the set variable doesn't pop the value from the stack because the set assignment
        // is an expression statement and there always is a pop instruction after expression statement.
        let val = *self.peek();
        let name = match self.chunk.get_constant(constant.index as usize) {
            Some(Value::Str(name)) => name,
            _ => panic!("Unreachable"),
        };

        match self.globals.get_mut(name) {
            Some(entry) => {
                *entry = val;
                Ok(())
            }
            None => Err(Error::runtime(
                *self.chunk.get_line(self.ip - 1).unwrap(),
                &format!("Undefined variable '{}'.", name),
            )),
        }
    }

    fn execute_constant(&mut self, con: Constant) -> VmResult<()> {
        let constant =
            self.chunk
                .get_constant(con.index as usize)
                .ok_or_else(|| Error::RuntimeError {
                    line: *self.chunk.get_line(self.ip - 1).unwrap(),
                    msg: String::from(""),
                })?;
        match constant {
            Value::Double(val) => self.push(StackValue::Num(*val)),
            Value::Bool(val) => self.push(StackValue::Bool(*val)),
            Value::Str(val) => self.allocate(HeapValue::Str(val.clone())),
            Value::Nil => self.push(StackValue::Nil),
        }
        Ok(())
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
            Err(msg) => Err(Error::runtime(
                *self.chunk.get_line(self.ip - 1).unwrap(),
                msg,
            )),
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
            (StackValue::Obj(l), StackValue::Obj(r)) => {
                let lobj = l.borrow();
                let robj = r.borrow();

                match (&lobj.value, &robj.value) {
                    (HeapValue::Str(lstr), HeapValue::Str(rstr)) => {
                        let mut res = String::with_capacity(lstr.len() + rstr.len());
                        res += lstr;
                        res += rstr;
                        Ok(StackOrHeap::Heap(HeapValue::Str(res)))
                    }
                    _ => Err("Expect both operands to be either numbers or strings."),
                }
            }
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

    fn get_line(&self) -> usize {
        *self.chunk.get_line(self.ip).unwrap()
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
        let sval = StackValue::Obj(UnsafeRef::new(&hval));

        self.objects.push_front(hval);
        self.stack.push(sval);
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::Compiler;
    use crate::error::Error;
    use crate::vm::VM;
    use losk_core::Scanner;
    use std::cell::RefCell;
    use std::rc::Rc;
    use std::str;

    fn test_program(src: &str, out: Option<&str>, err: Option<&str>) {
        println!("Testing source:\n{}", src);

        let mut scanner = Scanner::new();
        let compiler = Compiler::new();
        let chunk = compiler.compile(scanner.scan_tokens(src)).unwrap();
        let output: Rc<RefCell<Vec<u8>>> = Rc::new(RefCell::new(Vec::new()));

        let mut vm = VM::new(output.clone(), chunk);
        let result = vm.run();

        match (result, err) {
            (Err(Error::RuntimeError { msg, .. }), Some(err)) => assert_eq!(err, msg),
            (Err(Error::RuntimeError { msg, .. }), None) => {
                panic!("Not expecting any error, found '{}'", msg)
            }
            (Ok(_), Some(err)) => panic!("Expecting an error '{}', found none.", err),
            _ => {}
        }

        if let Some(out) = out {
            assert_eq!(str::from_utf8(&output.borrow()).unwrap(), out);
        }
        println!("\n\n");
    }

    #[test]
    fn test_programs() {
        let tests = [
            (
                include_str!("../../data/print_expression.lox"),
                include_str!("../../data/print_expression.lox.expected"),
            ),
            (
                include_str!("../../data/var_assignment.lox"),
                include_str!("../../data/var_assignment.lox.expected"),
            ),
            (
                include_str!("../../data/block.lox"),
                include_str!("../../data/block.lox.expected"),
            ),
            (
                include_str!("../../data/if_else.lox"),
                include_str!("../../data/if_else.lox.expected"),
            ),
            (
                include_str!("../../data/while.lox"),
                include_str!("../../data/while.lox.expected"),
            ),
        ];

        for (src, expected) in tests {
            test_program(src, Some(expected), None);
        }
    }
}
