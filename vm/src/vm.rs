use crate::chunk::{Chunk, Instruction};
use crate::error::Error;
use crate::instruction::Constant;
use crate::value::Value;
use crate::VmResult;
use std::ops::{Add, Div, Mul, Neg, Not, Sub};

const DEFAULT_STACK: usize = 256;

pub(crate) struct VM {
    // instruction pointer, this will be incremented during interpretation
    ip: usize,

    // The stack as a growable vector. In textbook, this is represented by an array and a stack
    // pointer that always point to one element past the top. This is not required here since the
    // vector already give us those functionality.
    stack: Vec<Value>,

    // Probably would be better to use a context object for compiling, so I dun have to pass
    // the chunk around, or take ownership of it in the VM. But currently, creating a VM entails
    // creating a new stack as the most heavy operation so it's not a big deal yet.
    // As the initialisation task gets heavier, the scale will start tipping more to reusing
    // existing VM and resetting the stack (and ip) after runtime errors.
    // Also it would be cool to support some sort of debugger, where the user can walk the chunk
    // instructions as the VM executes them.
    chunk: Chunk,
}

impl VM {
    pub(crate) fn new(chunk: Chunk) -> Self {
        VM {
            ip: 0,
            stack: Vec::with_capacity(DEFAULT_STACK),
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
                Instruction::LiteralTrue => self.push(Value::Bool(true)),
                Instruction::LiteralFalse => self.push(Value::Bool(false)),
                Instruction::LiteralNil => self.push(Value::Nil),
                Instruction::Equal => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(Value::Bool(lhs == rhs));
                }
                Instruction::Greater => self.execute_binary_op(Value::greater)?,
                Instruction::Less => self.execute_binary_op(Value::greater)?,
                Instruction::Negate => {
                    let res = self.pop().neg();
                    self.push(self.value_or_err(res)?);
                }
                Instruction::Not => {
                    let res = self.pop().not();
                    self.push(self.value_or_err(res)?);
                }
                Instruction::Add => self.execute_binary_op(Value::add)?,
                Instruction::Subtract => self.execute_binary_op(Value::sub)?,
                Instruction::Multiply => self.execute_binary_op(Value::mul)?,
                Instruction::Divide => self.execute_binary_op(Value::div)?,
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
        self.push(*constant);
        Ok(())
    }

    fn execute_binary_op(
        &mut self,
        op: fn(Value, Value) -> Result<Value, &'static str>,
    ) -> VmResult<()> {
        let rhs = self.pop();
        let lhs = self.pop();
        self.push(self.value_or_err(op(lhs, rhs))?);
        Ok(())
    }

    fn get_line(&self) -> usize {
        *self.chunk.get_line(self.ip).unwrap()
    }

    fn push(&mut self, val: Value) {
        self.stack.push(val)
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }

    fn value_or_err(&self, res: Result<Value, &'static str>) -> Result<Value, Error> {
        match res {
            Ok(val) => Ok(val),
            Err(msg) => Err(Error::runtime(
                *self.chunk.get_line(self.ip - 1).unwrap(),
                msg,
            )),
        }
    }
}
