use crate::instruction::Constant;
use crate::value::Value;

// These instructions need to be able to turn into opcodes (format unknown yet.)
// And vice versa in the disassembler.
// The instructions are an array of bytes with no padding for instruction, i.e if the instruction
// is `return`, there will be no padding and the next instruction starts from the next immediate byte.
//
// The chunk object also needs another integer array to store the line number of the source location
// where the instruction comes from.
#[derive(Debug)]
pub(crate) enum Instruction {
    Constant(Constant),
    Add,
    Subtract,
    Multiply,
    Divide,
    Negate,
    Return,
}

#[derive(Debug)]
pub(crate) struct Chunk {
    instructions: Vec<Instruction>,
    line_numbers: Vec<usize>,
    constants: Vec<Value>,
}

impl Chunk {
    pub(crate) fn new() -> Self {
        Chunk {
            instructions: Vec::new(),
            line_numbers: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub(crate) fn add_instruction(&mut self, instruction: Instruction, line_number: usize) {
        self.instructions.push(instruction);
        self.line_numbers.push(line_number);
    }

    pub(crate) fn add_constant(
        &mut self,
        value: Value,
        line_number: usize,
    ) -> Result<u8, &'static str> {
        if self.constants.len() == u8::MAX as usize {
            return Err("too many constants in one chunk.");
        }

        self.instructions.push(Instruction::Constant(Constant {
            index: self.constants.len() as u8,
        }));
        self.constants.push(value);
        self.line_numbers.push(line_number);
        Ok(self.constants.len() as u8)
    }

    pub(crate) fn get_line(&self, offset: usize) -> Option<&usize> {
        self.line_numbers.get(offset)
    }

    pub(crate) fn get_instruction(&self, offset: usize) -> Option<&Instruction> {
        self.instructions.get(offset)
    }

    pub(crate) fn get_constant(&self, index: usize) -> Option<&Value> {
        self.constants.get(index)
    }
}
