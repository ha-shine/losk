# Todos

- [ ] Write a disassembler that will print offset and opcodes. Not sure how this will be useful yet. The disassembler
      should print the codes in this format `<offset> <line_number/'|' if same line as previous> <instruction>`

# Challenges

- [ ] The line numbers are stored in corresponding line number array where each instruction will have an integer that
      indicates which line that instruction belongs to. This is wasteful, come up with a better compression algorithm.
      Run-length encoding? Hint: It's not necessary for decoding line number to be efficient since it's only called
      when runtime error occurs. (or in disassembler)
- [ ] Currently `OpCode::Constant` stores u8 index which only allows 256 different constants. Create another op code
      which stores a different sized index to allow bigger constant pool