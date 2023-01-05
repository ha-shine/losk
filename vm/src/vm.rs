use crate::chunk::{Chunk, Instruction};
use crate::error::Error;
use crate::instruction::Constant;
use crate::value::Value;
use crate::Result;
use std::borrow::Cow;

const STACK_MAX: usize = 256;

pub(crate) struct VM {
    // instruction pointer, this will be incremented during interpretation
    ip: usize,

    // the stack and the stack pointer that always point to one past the last element
    stack: [Value; STACK_MAX],
    sp: usize,
}

impl VM {
    pub(crate) fn new() -> Self {
        VM {
            ip: 0,
            stack: [Value::Empty; STACK_MAX],
            sp: 0,
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
                        msg: Cow::from("What ever"),
                    })?;

            #[cfg(feature = "debug-trace-execution")]
            for i in 0..self.sp {
                println!("[ {:10} ]", self.stack[i]);
            }

            // TODO: Disassemble this instruction if DEBUG_TRACE_EXECUTION is defined. But I guess
            //   I don't need it here because the instructions are already disassembled into
            //   `Instruction` object. Maybe the textbook will not do the disassembling ahead of
            //   time and operate on a byte stream?
            self.ip += 1;

            match instruction {
                Instruction::Constant(val) => self.execute_constant(chunk, val)?,
                Instruction::Return => {
                    self.pop();
                }
                _ => {}
            }
        }
    }

    fn execute_constant(&mut self, chunk: &Chunk, instruction: &Constant) -> Result<()> {
        let constant = chunk
            .get_constant(instruction.index as usize)
            .ok_or_else(|| Error::RuntimeError {
                line: *chunk.get_line(self.ip - 1).unwrap(),
                msg: Cow::from(""),
            });
        Ok(())
    }

    fn push(&mut self, val: Value) {
        self.stack[self.sp] = val;
        self.sp += 1;
    }

    fn pop(&mut self) -> Value {
        self.sp -= 1;
        self.stack[self.sp]
    }
}
