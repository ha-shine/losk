use crate::chunk::{Chunk, Instruction};
use crate::error::Error;
use crate::instruction::Constant;
use crate::value::Value;
use crate::Result;
use std::borrow::Cow;
use std::ops::{Add, Div, Mul, Sub};

const DEFAULT_STACK: usize = 256;

pub(crate) struct VM {
    // instruction pointer, this will be incremented during interpretation
    ip: usize,

    // The stack as a growable vector. In textbook, this is represented by an array and a stack
    // pointer that always point to one element past the top. This is not required here since the
    // vector already give us those functionality.
    stack: Vec<Value>,
}

impl VM {
    pub(crate) fn new() -> Self {
        VM {
            ip: 0,
            stack: Vec::with_capacity(DEFAULT_STACK),
        }
    }

    pub(crate) fn interpret(&mut self, chunk: &Chunk) -> Result<()> {
        self.ip = 0;
        Ok(())
    }

    fn run(&mut self, chunk: &Chunk) -> Result<()> {
        loop {
            let instruction =
                chunk
                    .get_instruction(self.ip)
                    .ok_or_else(|| Error::RuntimeError {
                        line: *chunk.get_line(self.ip).unwrap(),
                        msg: String::from("What ever"),
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
                Instruction::Constant(val) => self.execute_constant(chunk, val)?,
                Instruction::Negate => {
                    let top = self.pop();
                    self.push(-top);
                }
                Instruction::Add => self.execute_binary_op(Value::add),
                Instruction::Subtract => self.execute_binary_op(Value::sub),
                Instruction::Multiply => self.execute_binary_op(Value::mul),
                Instruction::Divide => self.execute_binary_op(Value::div),
                Instruction::Return => {
                    self.pop();
                }
            }
        }
    }

    fn execute_constant(&mut self, chunk: &Chunk, instruction: &Constant) -> Result<()> {
        let constant = chunk
            .get_constant(instruction.index as usize)
            .ok_or_else(|| Error::RuntimeError {
                line: *chunk.get_line(self.ip - 1).unwrap(),
                msg: String::from(""),
            });
        Ok(())
    }

    fn execute_binary_op(&mut self, op: fn(Value, Value) -> Value) {
        let rhs = self.pop();
        let lhs = self.pop();
        self.push(op(lhs, rhs));
    }

    fn push(&mut self, val: Value) {
        self.stack.push(val)
    }

    fn pop(&mut self) -> Value {
        self.stack.pop().unwrap()
    }
}
