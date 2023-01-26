# Todos

- [ ] Write a disassembler that will print offset and opcodes. Not sure how this will be useful yet. The disassembler
      should print the codes in this format `<offset> <line_number/'|' if same line as previous> <instruction>`.
- [ ] Currently, function parsing is a bit weird in a way that synchronisation is not skipped to the end of the block
      if an error is found, but the parsing stop in the middle of the function. Try parsing an error function and see
      if the synchronisation makes sense.
- [ ] Explore "closure conversion" or "lambda lifting" for faster closures.

# Challenges

- [ ] The line numbers are stored in corresponding line number array where each instruction will have an integer that
      indicates which line that instruction belongs to. This is wasteful, come up with a better compression algorithm.
      Run-length encoding? Hint: It's not necessary for decoding line number to be efficient since it's only called
      when runtime error occurs. (or in disassembler).
- [ ] Currently `OpCode::Constant` stores u8 index which only allows 256 different constants. Create another op code
      which stores a different sized index to allow bigger constant pool.
- [ ] The program will always create a new constant whenever one is encountered. That means a constant is created everytime
      a variable is declared, or referred further in the code even if it has already been declared. It's easy to
      optimize this by using a map to avoid creating new constants for those already been created. But this means, during
      runtime, the constants will be accessed out of order and cache efficiency will drop.
- [ ] Currently there's no way for native function to signal a runtime error, support this feature.