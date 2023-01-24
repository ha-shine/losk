use crate::value::ConstantValue;

// These instructions need to be able to turn into opcodes (format unknown yet.)
// And vice versa in the disassembler.
// The instructions are an array of bytes with no padding for instruction, i.e if the instruction
// is `return`, there will be no padding and the next instruction starts from the next immediate byte.
//
// The chunk object also needs another integer array to store the line number of the source location
// where the instruction comes from.
#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) enum Instruction {
    Constant(Constant),
    LiteralTrue,
    LiteralFalse,
    LiteralNil,
    Pop,
    GetGlobal(Constant),
    DefineGlobal(Constant),
    SetGlobal(Constant),
    GetLocal(StackPosition),
    SetLocal(StackPosition),
    GetUpvalue(UpvalueIndex),
    SetUpvalue(UpvalueIndex),
    SetProperty(Constant),
    GetProperty(Constant),
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,
    JumpIfFalse(JumpDist),
    Jump(JumpDist),
    Loop(JumpDist),
    Call(ArgCount),
    Closure(Constant),
    CloseUpvalue,
    Return,
    Class(Constant),
    Method(Constant),
}

// The position of a value on the stack represented in different ways -
// - Offset is how far the value is from the beginning of that call frame
// - Index is the absolute position starting from index 0
// - RevOffset is the same as offset, but in reverse so the actual position is len - offset - 1
// They are interchangeable but need the context of a VM
#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) enum StackPosition {
    Offset(usize),
    Index(usize),
    RevOffset(usize),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) struct Constant(pub(crate) u8);

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) struct JumpDist(pub(crate) usize);

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) struct ArgCount(pub(crate) usize);

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) struct UpvalueIndex(pub(crate) usize);

#[derive(Clone, Debug, PartialEq)]
pub(crate) struct Chunk {
    instructions: Vec<Instruction>,
    line_numbers: Vec<usize>,
    constants: Vec<ConstantValue>,
}

impl Chunk {
    pub(crate) fn new() -> Self {
        Chunk {
            instructions: Vec::new(),
            line_numbers: Vec::new(),
            constants: Vec::new(),
        }
    }

    pub(crate) fn add_instruction(
        &mut self,
        instruction: Instruction,
        line_number: usize,
    ) -> usize {
        self.instructions.push(instruction);
        self.line_numbers.push(line_number);
        self.instructions.len() - 1
    }

    pub(crate) fn make_constant(&mut self, value: ConstantValue) -> Result<Constant, &'static str> {
        if self.constants.len() == u8::MAX as usize {
            return Err("too many constants in one chunk");
        }

        self.constants.push(value);

        let index = (self.constants.len() as u8) - 1;
        Ok(Constant(index))
    }

    pub(crate) fn patch_jump(&mut self, index: usize) {
        let jump = self.instructions.len() - index;
        match self.instructions.get_mut(index) {
            Some(Instruction::JumpIfFalse(JumpDist(dist))) => {
                *dist = jump;
            }
            Some(Instruction::Jump(JumpDist(dist))) => {
                *dist = jump;
            }
            _ => panic!("Unreachable!"),
        }
    }

    pub(crate) fn in_count(&self) -> usize {
        self.instructions.len()
    }

    pub(crate) fn get_line(&self, offset: usize) -> Option<&usize> {
        self.line_numbers.get(offset)
    }

    pub(crate) fn get_instruction(&self, offset: usize) -> Option<&Instruction> {
        self.instructions.get(offset)
    }

    pub(crate) fn get_constant(&self, index: usize) -> Option<&ConstantValue> {
        self.constants.get(index)
    }
}
