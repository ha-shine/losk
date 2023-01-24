use losk_core::{Token, TokenStream, Type};

use crate::chunk::{Constant, Instruction};
use crate::compiler::*;
use crate::object::Upvalue;
use crate::value::ConstantValue;
use crate::{CompileError, Compiler, Function};

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

    pub(super) fn add_local(
        &mut self,
        name: String,
        depth: Option<isize>,
    ) -> CompilationResult<()> {
        if self.locals.len() == Compiler::STACK_SIZE {
            Err(self.error("Too many local variables in function."))
        } else {
            self.locals.push(Local {
                name,
                depth: depth.unwrap_or(-1),
                is_captured: false,
            });
            Ok(())
        }
    }

    // Resolve the given variable by traversing through all scopes.
    //   - If there is no enclosing scope, this is the global scope so the variable is a global variable.
    //   - If the variable can be resolved locally by walking outward from current scope, it's a
    //     local variable. Note that the scope here means a block of statements, not a function
    //     or class.
    //   - Variables that are found across context boundaries are upvalues.
    //
    // It's possible to capture an upvalue that is already captured by enclosing function. In that
    // case, an non-local upvalue is created.
    pub(super) fn resolve_variable(&mut self, name: &str) -> CompilationResult<LocalResolution> {
        if let Some(val) = self.resolve_local(name)? {
            return Ok(LocalResolution::Local(val));
        }

        // Can't be found in local, and I need to check the parent function if this could be an
        // upvalue. If there are no parents, the variable must be global.
        if self.enclosing.is_none() {
            return Ok(LocalResolution::Global);
        }

        // Check if the variable is defined in local scope
        if let Some(val) = self.resolve_local(name)? {
            return Ok(LocalResolution::Local(val));
        }

        let result = match self.enclosing.as_mut().unwrap().resolve_variable(name)? {
            // It's a local variable from enclosing, so captured it as an upvalue in the current
            // function and mark it in the enclosing function as captured.
            LocalResolution::Local(local) => {
                self.enclosing.as_mut().unwrap().locals[local.0].is_captured = true;
                self.fun
                    .add_upvalue(Upvalue::new(local.0, true))
                    .map(LocalResolution::UpValue)
            }

            // It's already captured in the enclosing function, capture it again as an upvalue
            LocalResolution::UpValue(upvalue) => self
                .fun
                .add_upvalue(Upvalue::new(upvalue.0, false))
                .map(LocalResolution::UpValue),

            // Enclosing variable report it as a global, so mark it as one
            // TODO: Maybe I can just check if the name is `this` and throw an error here?
            LocalResolution::Global => Ok(LocalResolution::Global),
        };

        self.unwrap_result(result)
    }

    fn resolve_local(&self, name: &str) -> CompilationResult<Option<LocalIndex>> {
        for (idx, local) in self.locals.iter().enumerate().rev() {
            if local.name == name {
                return if local.depth == -1 {
                    Err(self.error("Can't read local variable in its own initializer"))
                } else {
                    Ok(Some(LocalIndex(idx)))
                };
            }
        }

        Ok(None)
    }

    pub(super) fn identifier_constant(&mut self, name: String) -> CompilationResult<Constant> {
        let res = self.fun.chunk.make_constant(ConstantValue::Str(name));
        self.unwrap_result(res)
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

        while !self.locals.is_empty() && self.locals.last().unwrap().depth > self.scope_depth {
            // If the local is captured by an inner function, that variable needs to be hoisted on
            // to the heap
            let last = self.locals.pop().unwrap();
            if last.is_captured {
                self.add_instruction(Instruction::CloseUpvalue);
            } else {
                self.add_instruction(Instruction::Pop);
            }
        }
    }

    pub(super) fn add_return(&mut self) {
        match self.ftype {
            FunctionType::Initializer => {
                self.add_instruction(Instruction::GetLocal(StackPosition::Offset(0)))
            }
            _ => self.add_instruction(Instruction::LiteralNil),
        };

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

    pub(super) fn unwrap_result<T>(&self, res: Result<T, &'static str>) -> CompilationResult<T> {
        match res {
            Ok(val) => Ok(val),
            Err(msg) => Err(self.error(msg)),
        }
    }
}
