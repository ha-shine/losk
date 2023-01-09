use crate::chunk::{Chunk, Instruction};
use crate::error::Error;
use crate::instruction::Constant;
use crate::value::Value;
use crate::VmResult;
use std::fmt::{Display, Formatter};

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
    Obj(*const Object),
    Nil,
}

impl Display for StackValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StackValue::Num(val) => write!(f, "{}", val),
            StackValue::Bool(val) => write!(f, "{}", val),
            StackValue::Obj(val) => write!(f, "{:p}", val),
            StackValue::Nil => write!(f, "nil"),
        }
    }
}

#[allow(dead_code)]
pub(crate) enum ObjectValue {
    Str(String),
}

#[allow(dead_code)]
pub(crate) struct Object {
    value: ObjectValue,
}

#[allow(dead_code)]
pub(crate) struct VM<'a> {
    // instruction pointer, this will be incremented during interpretation
    ip: usize,

    // The stack as a growable vector. In textbook, this is represented by an array and a stack
    // pointer that always point to one element past the top. This is not required here since the
    // vector already give us those functionality.
    stack: Vec<StackValue>,

    // List of objects currently allocated on the heap
    objects: Vec<Object>,

    // Probably would be better to use a context object for compiling, so I dun have to pass
    // the chunk around, or take ownership of it in the VM. But currently, creating a VM entails
    // creating a new stack as the most heavy operation so it's not a big deal yet.
    // As the initialisation task gets heavier, the scale will start tipping more to reusing
    // existing VM and resetting the stack (and ip) after runtime errors.
    // Also it would be cool to support some sort of debugger, where the user can walk the chunk
    // instructions as the VM executes them.
    chunk: Chunk<'a>,
}

type OpResult = Result<(StackValue, Option<Object>), &'static str>;

#[allow(dead_code)]
impl<'a> VM<'a> {
    pub(crate) fn new(chunk: Chunk<'a>) -> Self {
        VM {
            ip: 0,
            stack: Vec::with_capacity(DEFAULT_STACK),
            objects: Vec::new(),
            chunk,
        }
    }

    pub(crate) fn interpret(&mut self) -> VmResult<()> {
        self.ip = 0;
        Ok(())
    }

    fn run(&mut self) -> VmResult<()> {
        loop {
            let instruction = self.chunk.get_instruction(self.ip).ok_or_else(|| {
                Error::runtime(*self.chunk.get_line(self.ip).unwrap(), "What ever")
            })?;

            #[cfg(feature = "debug-trace-execution")]
            for val in &self.stack {
                println!("[ {:10} ]", val);
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
                Instruction::Return => {
                    self.pop();
                }
            }
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
        // self.push(*constant);
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
            Ok((stack, heap)) => {
                self.stack.push(stack);
                if let Some(val) = heap {
                    // TODO Add heap value
                }
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
            (StackValue::Num(l), StackValue::Num(r)) => Ok((StackValue::Num(l + r), None)),
            (StackValue::Obj(l), StackValue::Num(r)) => Ok((StackValue::Nil, None)), // TODO: implement
            (_, _) => Err("Expect both operands to be either numbers or strings."),
        }
    }

    fn sub(&mut self, lhs: StackValue, rhs: StackValue) -> OpResult {
        match (lhs, rhs) {
            (StackValue::Num(l), StackValue::Num(r)) => Ok((StackValue::Num(l - r), None)),
            (_, _) => Err("Expect both operands to be numbers."),
        }
    }

    fn mul(&mut self, lhs: StackValue, rhs: StackValue) -> OpResult {
        match (lhs, rhs) {
            (StackValue::Num(l), StackValue::Num(r)) => Ok((StackValue::Num(l * r), None)),
            (_, _) => Err("Expect both operands to be numbers."),
        }
    }

    fn div(&mut self, lhs: StackValue, rhs: StackValue) -> OpResult {
        match (lhs, rhs) {
            (StackValue::Num(l), StackValue::Num(r)) => Ok((StackValue::Num(l / r), None)),
            (_, _) => Err("Expect both operands to be numbers."),
        }
    }

    fn greater(&mut self, lhs: StackValue, rhs: StackValue) -> OpResult {
        match (lhs, rhs) {
            (StackValue::Num(l), StackValue::Num(r)) => Ok((StackValue::Bool(l > r), None)),
            (_, _) => Err("Expect both operands to be numbers."),
        }
    }

    fn less(&mut self, lhs: StackValue, rhs: StackValue) -> OpResult {
        match (lhs, rhs) {
            (StackValue::Num(l), StackValue::Num(r)) => Ok((StackValue::Bool(l < r), None)),
            (_, _) => Err("Expect both operands to be numbers."),
        }
    }

    fn not(&mut self, val: StackValue) -> OpResult {
        match val {
            StackValue::Bool(val) => Ok((StackValue::Bool(!val), None)),
            _ => Err("Expect operand to be a boolean value."),
        }
    }

    fn neg(&mut self, val: StackValue) -> OpResult {
        match val {
            StackValue::Num(val) => Ok((StackValue::Num(-val), None)),
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
}
