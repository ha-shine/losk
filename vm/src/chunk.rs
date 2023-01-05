use crate::instruction::Constant;
use crate::value::Value;

#[repr(u8)]
pub(crate) enum OpCode {
    Constant = 0,
    Return = 1,
}

impl TryInto<OpCode> for u8 {
    type Error = ();

    fn try_into(self) -> Result<OpCode, Self::Error> {
        match self {
            0 => Ok(OpCode::Constant),
            1 => Ok(OpCode::Return),
            _ => Err(()),
        }
    }
}

// These instructions need to be able to turn into opcodes (format unknown yet.)
// And vice versa in the disassembler.
// The instructions are an array of bytes with no padding for instruction, i.e if the instruction
// is `return`, there will be no padding and the next instruction starts from the next immediate byte.
//
// The chunk object also needs another integer array to store the line number of the source location
// where the instruction comes from.
pub(crate) enum Instruction {
    Constant(Constant),
    Return,
}

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
