use crate::chunk::{Constant, Instruction};
use crate::compiler::*;
use crate::value::Value;
use crate::{CompileError, Compiler, Function};
use losk_core::{Token, TokenStream, Type};

pub(super) struct Context<'token> {
    pub(super) stream: TokenStream<'token>,
    pub(super) fun: Function,

    pub(super) curr: Option<Token>,
    pub(super) prev: Option<Token>,

    pub(super) errs: Vec<CompileError>,

    // locals are used to resolve the declared variables into a stack frame location. `scope_depth`
    // is an auxiliary data that is used in this task, by tracking which level of scope the compiler
    // is currently in.
    // The textbook uses a pointer to token for local's name, but in this implementation, the tokens
    // are immediately discarded after usage and I can't keep their pointers. So their names will
    // be copied instead.
    pub(super) locals: Vec<Local>,
    pub(super) scope_depth: isize,

    pub(super) ftype: FunctionType,
    pub(super) enclosing: Option<Box<Context<'token>>>,
}

impl<'token> Context<'token> {
    pub(super) fn new(
        stream: TokenStream<'token>,
        ftype: FunctionType,
        enclosing: Option<Box<Context<'token>>>,
    ) -> Self {
        Context {
            stream,
            fun: Function::new("<script>", 0),

            curr: None,
            prev: None,

            errs: Vec::new(),

            locals: Vec::new(),
            scope_depth: 0,

            ftype,
            enclosing,
        }
    }

    pub(super) fn add_local(&mut self, name: String) -> CompilationResult<()> {
        if self.locals.len() == Compiler::STACK_SIZE {
            Err(self.error("Too many local variables in function."))
        } else {
            self.locals.push(Local { name, depth: -1 });
            Ok(())
        }
    }

    // Resolve the given variable by traversing through all scopes.
    //   - If there is no enclosing scope, this is the global scope so the variable is a global variable.
    //   - If the variable can be resolved locally by walking outward from current scope, it's a
    //     local variable. Note that the scope here means a block of statements, not a function
    //     or class.
    //   - Variables that are found across context boundaries are upvalues.
    pub(super) fn resolve_variable(&mut self, name: &str) -> CompilationResult<LocalResolution> {
        if let Some(val) = self.resolve_local(name)? {
            return Ok(LocalResolution::Local(val));
        }

        // Can't be found in local, and I need to check the parent function if this could be an
        // upvalue. If there are no parents, the variable must be global.
        if self.enclosing.is_none() {
            return Ok(LocalResolution::Global);
        }

        match self.resolve_local(name)? {
            Some(val) => Ok(LocalResolution::Local(val)),
            None => match self.enclosing.as_ref().unwrap().resolve_local(name)? {
                Some(upval) => {
                    todo!()
                }
                None => Ok(LocalResolution::Global),
            },
        }
    }

    pub(super) fn resolve_local(&self, name: &str) -> CompilationResult<Option<LocalValue>> {
        for (idx, local) in self.locals.iter().rev().enumerate() {
            if local.name == name {
                return if local.depth == -1 {
                    Err(self.error("Can't read local variable in its own initializer"))
                } else {
                    Ok(Some(LocalValue { index: idx }))
                };
            }
        }

        Ok(None)
    }

    pub(super) fn identifier_constant(&mut self, name: String) -> Constant {
        self.fun.chunk.make_constant(Value::Str(name)).unwrap()
    }

    pub(super) fn define_variable(&mut self, constant: Constant, line: usize) {
        // For local scope, the variable doesn't even need to be defined. They can be referred
        // by scope index.
        if self.scope_depth > 0 {
            self.mark_initialized();
            return;
        }

        self.add_instruction_from(Instruction::DefineGlobal(constant), line);
    }

    pub(super) fn mark_initialized(&mut self) {
        if self.scope_depth == 0 {
            return;
        }

        self.locals.last_mut().unwrap().depth = self.scope_depth;
    }

    pub(super) fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    pub(super) fn end_scope(&mut self) {
        self.scope_depth -= 1;

        let mut n = 0;
        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.scope_depth {
            self.locals.pop();
            n += 1;
        }

        if n > 0 {
            self.add_instruction(Instruction::PopN(n));
        }
    }

    pub(super) fn add_return(&mut self) {
        self.add_instruction(Instruction::LiteralNil);
        self.add_instruction(Instruction::Return);
    }

    pub(super) fn add_instruction(&mut self, instruction: Instruction) -> usize {
        self.add_instruction_from(instruction, self.prev.as_ref().unwrap().line)
    }

    pub(super) fn add_instruction_from(
        &mut self,
        instruction: Instruction,
        from_line: usize,
    ) -> usize {
        self.fun.chunk.add_instruction(instruction, from_line)
    }

    pub(super) fn advance(&mut self) {
        self.prev = self.curr.take();
        self.curr = self.stream.next();
    }

    // Consume will return the line number of the consumed token for ease.
    // Maybe I should return the reference to the token as a result instead of just the line number.
    // This way, I can bubble up the result back to the main compile function too.
    pub(super) fn consume(&mut self, ty: Type, msg: &str) -> CompilationResult<usize> {
        match &self.curr {
            Some(token) if token.ty == ty => {
                let line = token.line;
                self.advance();
                Ok(line)
            }
            _ => Err(self.error(msg)),
        }
    }

    pub(super) fn match_type(&mut self, ty: Type) -> bool {
        if !self.check(ty) {
            false
        } else {
            self.advance();
            true
        }
    }

    pub(super) fn emit_loop(&mut self, start: usize, line: usize) {
        let dist = self.fun.chunk.in_count() - start;
        self.add_instruction_from(Instruction::Loop(JumpDist(dist)), line);
    }

    pub(super) fn check(&self, ty: Type) -> bool {
        self.curr.as_ref().unwrap().ty == ty
    }

    pub(super) fn error(&self, msg: &str) -> CompileError {
        let line = if let Some(curr) = &self.curr {
            curr.line
        } else {
            0
        };

        CompileError::new(line, msg)
    }
}
