use crate::chunk::Instruction;
use crate::error::{CompilerResult, Error};
use crate::instruction::{ArgCount, Constant, JumpDist, StackOffset};
use crate::object::Function;
use crate::value::Value;
use losk_core::{Token, TokenStream, Type};

pub(crate) struct Compiler;

#[derive(Copy, Clone)]
enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    fn next(&self) -> Self {
        match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => Precedence::None, // What should be returned here?
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
enum FunctionType {
    Script,
    Function,
}

impl Compiler {
    pub(crate) fn new() -> Self {
        Compiler
    }

    pub(crate) fn compile(&self, stream: TokenStream) -> Result<Function, Vec<Error>> {
        let ctx = Context::compiled(stream, FunctionType::Script);

        if !ctx.errs.is_empty() {
            Err(ctx.errs)
        } else {
            Ok(ctx.fun)
        }
    }
}

struct Local {
    name: String,
    depth: isize,
}

struct Context<'a> {
    stream: TokenStream<'a>,
    fun: Function,

    curr: Option<Token>,
    prev: Option<Token>,

    errs: Vec<Error>,

    // locals are used to resolve the declared variables into a stack frame location. `scope_depth`
    // is an auxiliary data that is used in this task, by tracking which level of scope the compiler
    // is currently in.
    // The textbook uses a pointer to token for local's name, but in this implementation, the tokens
    // are immediately discarded after usage and I can't keep their pointers. So their names will
    // be copied instead.
    locals: Vec<Local>,
    scope_depth: isize,

    ftype: FunctionType,
}

type ParseFn<'a> = fn(&mut Context<'a>, bool) -> CompilerResult<()>;

struct ParseRule<'a>(Option<ParseFn<'a>>, Option<ParseFn<'a>>, Precedence);

impl<'a> ParseRule<'a> {
    fn prefix(&self) -> Option<ParseFn<'a>> {
        self.0
    }

    fn infix(&self) -> Option<ParseFn<'a>> {
        self.1
    }

    fn precedence(&self) -> &Precedence {
        &self.2
    }
}

impl<'a> Context<'a> {
    const STACK_SIZE: usize = u8::MAX as usize + 1;

    // A Pratt parser's table, where each row map to token type as index. The first column
    // maps a token type to prefix parsing function while the second column maps to infix parsing
    // function. The third column precedence represents the precedence of the infix expression
    // which uses that token as an operator.
    // Precedence of the infix operators don't need to be tracked since the precedence of all prefix
    // operators are the same.
    // I am not sure whether using this as a const is the best idea since the const in Rust is
    // like `#define` and the code is copied to every usage.
    const PARSE_RULES: [ParseRule<'a>; 39] = [
        ParseRule(Some(Self::grouping), Some(Self::call), Precedence::Call), // LeftParen
        ParseRule(None, None, Precedence::None),                             // RightParen
        ParseRule(None, None, Precedence::None),                             // LeftBrace
        ParseRule(None, None, Precedence::None),                             // RightBrace
        ParseRule(None, None, Precedence::None),                             // Comma
        ParseRule(None, None, Precedence::None),                             // Dot
        ParseRule(Some(Self::unary), Some(Self::binary), Precedence::Term),  // Minus
        ParseRule(None, Some(Self::binary), Precedence::Term),               // Plus
        ParseRule(None, None, Precedence::None),                             // Semicolon
        ParseRule(None, Some(Self::binary), Precedence::Factor),             // Slash
        ParseRule(None, Some(Self::binary), Precedence::Factor),             // Star
        ParseRule(Some(Self::unary), None, Precedence::None),                // Bang
        ParseRule(None, Some(Self::binary), Precedence::Equality),           // BangEqual
        ParseRule(None, None, Precedence::None),                             // Equal
        ParseRule(None, Some(Self::binary), Precedence::Equality),           // EqualEqual
        ParseRule(None, Some(Self::binary), Precedence::Comparison),         // Greater
        ParseRule(None, Some(Self::binary), Precedence::Comparison),         // GreaterEqual
        ParseRule(None, Some(Self::binary), Precedence::Comparison),         // Less
        ParseRule(None, Some(Self::binary), Precedence::Comparison),         // LessEqual
        ParseRule(Some(Self::variable), None, Precedence::None),             // Identifier
        ParseRule(Some(Self::string), None, Precedence::None),               // String
        ParseRule(Some(Self::number), None, Precedence::None),               // Number
        ParseRule(None, Some(Self::and), Precedence::And),                   // And
        ParseRule(None, None, Precedence::None),                             // Class
        ParseRule(None, None, Precedence::None),                             // Else
        ParseRule(Some(Self::literal), None, Precedence::None),              // True
        ParseRule(Some(Self::literal), None, Precedence::None),              // False
        ParseRule(None, None, Precedence::None),                             // For
        ParseRule(None, None, Precedence::None),                             // Fun
        ParseRule(None, None, Precedence::None),                             // If
        ParseRule(Some(Self::literal), None, Precedence::None),              // Nil
        ParseRule(None, Some(Self::or), Precedence::Or),                     // Or
        ParseRule(None, None, Precedence::None),                             // Print
        ParseRule(None, None, Precedence::None),                             // Return
        ParseRule(None, None, Precedence::None),                             // Super
        ParseRule(None, None, Precedence::None),                             // This
        ParseRule(None, None, Precedence::None),                             // Var
        ParseRule(None, None, Precedence::None),                             // While
        ParseRule(None, None, Precedence::None),                             // Error
    ];

    fn new(stream: TokenStream<'a>, ftype: FunctionType) -> Self {
        Context {
            stream,
            fun: Function::new("<script>", 0),

            curr: None,
            prev: None,

            errs: Vec::new(),

            locals: Vec::new(),
            scope_depth: 0,

            ftype,
        }
    }

    // Instead of having this function inside the context, this context should be passed mutably through
    // compiler's methods. This would make some of the multiple borrows restriction easier.
    fn compiled(stream: TokenStream<'a>, ftype: FunctionType) -> Self {
        let mut ctx = Context::new(stream, ftype);
        ctx.compile();
        ctx
    }

    fn compile(&mut self) {
        // If this is a function, the compiler doesn't need to parse until EOF. It's enough to
        // parse just the function name, parameters, and the body
        //
        // The return functions are always emitted whether the parsed function does return or
        // not. They will not be interpreted if the function return earlier anyway.
        if let FunctionType::Function = self.ftype {
            match self.compile_function() {
                Ok(_) => self.add_return(),
                Err(err) => {
                    self.errs.push(err);

                    // The synchronize here should be synchronized until the end of the block?
                    self.synchronize();
                }
            }
            return;
        }

        // Only advance to next character if the function being parsed is a script because
        // if function, the context needs access to previous token for naming the function.
        self.advance();

        // Maybe check the result at this level and do the synchronisation here?
        while !self.match_type(Type::Eof) {
            match self.declaration() {
                Ok(_) => {}
                Err(err) => {
                    self.errs.push(err);
                    self.synchronize();
                }
            }
        }

        self.add_return();
    }

    fn compile_function(&mut self) -> CompilerResult<()> {
        self.fun.name = self.prev.as_ref().unwrap().lexeme.clone();

        self.begin_scope();
        self.consume(Type::LeftParen, "Expect '(' after function name.")?;
        if !self.check(Type::RightParen) {
            loop {
                self.fun.arity += 1;
                if self.fun.arity > 255 {
                    return Err(self.error("Can't have more than 255 parameters."));
                }

                let constant = self.parse_variable("Expect parameter name")?;
                self.define_variable(constant, self.prev.as_ref().unwrap().line);

                if !self.match_type(Type::Comma) {
                    break;
                }
            }
        }

        self.consume(Type::RightParen, "Expect ')' after function parameters.")?;
        self.consume(Type::LeftBrace, "Expect '{' before function boyd.")?;
        self.block()?;
        Ok(())
    }

    fn declaration(&mut self) -> CompilerResult<()> {
        if self.match_type(Type::Var) {
            self.var_declaration()
        } else if self.match_type(Type::Fun) {
            self.fun_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> CompilerResult<()> {
        let constant = self.parse_variable("Expect variable name.")?;

        if self.match_type(Type::Equal) {
            self.expression()?;
        } else {
            self.add_instruction(Instruction::LiteralNil);
        }

        let line = self.consume(Type::SemiColon, "Expect ';' after variable declaration.")?;
        self.define_variable(constant, line);
        Ok(())
    }

    fn fun_declaration(&mut self) -> CompilerResult<()> {
        let global = self.parse_variable("Expect function name")?;
        self.mark_initialized();
        self.function(FunctionType::Function)?;
        self.define_variable(global, self.prev.as_ref().unwrap().line);
        Ok(())
    }

    fn statement(&mut self) -> CompilerResult<()> {
        if self.match_type(Type::Print) {
            self.print_statement()?;
        } else if self.match_type(Type::If) {
            self.if_statement()?;
        } else if self.match_type(Type::Return) {
            self.return_statement()?;
        } else if self.match_type(Type::While) {
            self.while_statement()?;
        } else if self.match_type(Type::For) {
            self.for_statement()?;
        } else if self.match_type(Type::LeftBrace) {
            self.begin_scope();
            self.block()?;
            self.end_scope();
        } else {
            self.expression_statement()?;
        }

        Ok(())
    }

    fn print_statement(&mut self) -> CompilerResult<()> {
        self.expression()?;
        let line = self.consume(Type::SemiColon, "Expect ';' after value.")?;
        self.add_instruction_from(Instruction::Print, line);
        Ok(())
    }

    fn return_statement(&mut self) -> CompilerResult<()> {
        if self.ftype == FunctionType::Script {
            return Err(self.error("Can't return from top-level code."));
        }

        if self.match_type(Type::SemiColon) {
            self.add_return();
        } else {
            self.expression()?;
            self.consume(Type::SemiColon, "Expect ';' after return value.")?;
            self.add_instruction(Instruction::Return);
        }

        Ok(())
    }

    fn if_statement(&mut self) -> CompilerResult<()> {
        self.consume(Type::LeftParen, "Expect '(' after 'if'.")?;
        self.expression()?;

        let mut line = self.consume(Type::RightParen, "Expect ')' after condition.")?;
        let then_jump =
            self.add_instruction_from(Instruction::JumpIfFalse(JumpDist { dist: 0 }), line);
        self.add_instruction_from(Instruction::Pop, line);
        self.statement()?;

        line = self.prev.as_ref().unwrap().line;
        let else_jump = self.add_instruction_from(Instruction::Jump(JumpDist { dist: 0 }), line);

        // Patching jump, here the if statement has been parsed and the program will jump to this
        // place if the test condition is false. In short,
        // then_jump = jump over the "then" statements after "if" when the condition is false
        // else_jump = jump over the "else" statements after executing if statements
        self.fun.chunk.patch_jump(then_jump);
        self.add_instruction_from(Instruction::Pop, line);

        if self.match_type(Type::Else) {
            self.statement()?;
        }

        // Need another jump patching here because without this patch, the if statement would just
        // fall through into else clause.
        self.fun.chunk.patch_jump(else_jump);
        Ok(())
    }

    fn while_statement(&mut self) -> CompilerResult<()> {
        let loop_start = self.fun.chunk.in_count();
        self.consume(Type::LeftParen, "Expect '(' after while.")?;
        self.expression()?;

        let mut line = self.consume(Type::RightParen, "Expect ')' after condition.")?;
        let exit_jump =
            self.add_instruction_from(Instruction::JumpIfFalse(JumpDist { dist: 0 }), line);
        self.add_instruction_from(Instruction::Pop, line);
        self.statement()?;

        line = self.prev.as_ref().unwrap().line;
        self.emit_loop(loop_start, line);
        self.fun.chunk.patch_jump(exit_jump);
        self.add_instruction(Instruction::Pop);

        Ok(())
    }

    fn for_statement(&mut self) -> CompilerResult<()> {
        // The for variable is scoped to the body, so a new scope is created.
        self.begin_scope();
        self.consume(Type::LeftParen, "Expect '(' after 'for'.")?;

        // The first part, initialization statement.
        if self.match_type(Type::SemiColon) {
            // No initializer
        } else if self.match_type(Type::Var) {
            self.var_declaration()?;
        } else {
            self.expression_statement()?;
        }

        // The second part, where the condition is tested before running the block.
        let mut loop_start = self.fun.chunk.in_count();
        let mut exit_jump = None;
        if !self.match_type(Type::SemiColon) {
            self.expression()?;
            let line = self.consume(Type::SemiColon, "Expect ';' after loop condition.")?;

            // This jump will be patched to skip the block if the condition is false.
            exit_jump = Some(
                self.add_instruction_from(Instruction::JumpIfFalse(JumpDist { dist: 0 }), line),
            );
            self.add_instruction_from(Instruction::Pop, line); // pop the condition
        }

        // The third part, incrementing the variable (or any expression statement - without ";").
        // This will be run after every block.
        if !self.match_type(Type::RightParen) {
            let body_jump = self.add_instruction(Instruction::Jump(JumpDist { dist: 0 }));
            let increment_start = self.fun.chunk.in_count();
            self.expression()?;
            self.add_instruction(Instruction::Pop);

            // After incrementing the variable, loop back to the start - which is the condition
            // testing instruction.
            let line = self.consume(Type::RightParen, "Expect ')' after for clauses.")?;
            self.emit_loop(loop_start, line);

            // The block is set to be looped back to the start - which normally is testing condition.
            // But if there's incrementing statement, it should be ran first before test condition.
            // It's achieved here by overriding the loop start index.
            loop_start = increment_start;

            // `body_jump` was emitted before the increment part, so essentially this is jumping
            // in this direction: test-condition -> block
            self.fun.chunk.patch_jump(body_jump);
        }

        // The block start
        self.statement()?;
        self.emit_loop(loop_start, self.prev.as_ref().unwrap().line);
        self.end_scope();

        if let Some(dist) = exit_jump {
            self.fun.chunk.patch_jump(dist);
            self.add_instruction(Instruction::Pop);
        }

        Ok(())
    }

    fn expression_statement(&mut self) -> CompilerResult<()> {
        self.expression()?;
        let line = self.consume(Type::SemiColon, "Expect ';' after expression.")?;
        self.add_instruction_from(Instruction::Pop, line);
        Ok(())
    }

    fn expression(&mut self) -> CompilerResult<()> {
        // Simply parse the expression with lowest precedence
        self.parse_precedence(Precedence::Assignment)?;
        Ok(())
    }

    fn block(&mut self) -> CompilerResult<()> {
        while !self.check(Type::RightBrace) && !self.check(Type::Eof) {
            self.declaration()?;
        }

        self.consume(Type::RightBrace, "Expect '}' after block.")?;
        Ok(())
    }

    // To parse a function, a new context is created and do the compilation as usual. The main
    // `compile` method will branch into the correct parsing method depending on the type.
    fn function(&mut self, ftype: FunctionType) -> CompilerResult<()> {
        let stream = std::mem::replace(&mut self.stream, TokenStream::new(""));
        let mut fun_ctx = Self::new(stream, ftype);
        fun_ctx.prev = self.prev.take();
        fun_ctx.curr = self.curr.take();
        fun_ctx.compile();

        // Replace the stream again
        let _ = std::mem::replace(&mut self.stream, fun_ctx.stream);
        self.prev = fun_ctx.prev;
        self.curr = fun_ctx.curr;

        if !fun_ctx.errs.is_empty() {
            self.errs.append(&mut fun_ctx.errs);
            Err(self.error("Error while parsing function"))
        } else {
            let fun_const = self
                .fun
                .chunk
                .make_constant(Value::Fun(fun_ctx.fun))
                .unwrap();
            self.add_instruction(Instruction::Constant(fun_const));
            Ok(())
        }
    }

    fn grouping(&mut self, _: bool) -> CompilerResult<()> {
        self.expression()?;
        self.consume(Type::RightParen, "Expect ')' after expression.")?;
        Ok(())
    }

    fn unary(&mut self, _: bool) -> CompilerResult<()> {
        let (ty, line) = {
            let prev = self.prev.as_ref().unwrap();
            (prev.ty, prev.line)
        };

        self.parse_precedence(Precedence::Unary)?;

        match ty {
            Type::Minus => {
                self.add_instruction_from(Instruction::Negate, line);
            }
            Type::Bang => {
                self.add_instruction_from(Instruction::Not, line);
            }
            _ => panic!("Unreachable"),
        }

        Ok(())
    }

    fn binary(&mut self, _: bool) -> CompilerResult<()> {
        let prev = self.prev.as_ref().unwrap();
        let ty = prev.ty;
        let line = prev.line;
        let rule = &Self::PARSE_RULES[ty as usize];

        // The second operand needs to be parsed with precedence higher than the current one.
        // Consider this expression - "2 + 3 * 10"
        // When the compiler have consumed the "+" sign, it needs to parse the whole expression
        // "3 * 10" first instead of just "3", else the operation order will be incorrect according
        // to the precedence rule.
        self.parse_precedence(rule.precedence().next())?;

        match ty {
            Type::Plus => {
                self.add_instruction_from(Instruction::Add, line);
            }
            Type::Minus => {
                self.add_instruction_from(Instruction::Subtract, line);
            }
            Type::Star => {
                self.add_instruction_from(Instruction::Multiply, line);
            }
            Type::Slash => {
                self.add_instruction_from(Instruction::Divide, line);
            }
            Type::BangEqual => {
                self.add_instruction_from(Instruction::Equal, line);
                self.add_instruction_from(Instruction::Not, line);
            }
            Type::EqualEqual => {
                self.add_instruction_from(Instruction::Equal, line);
            }
            Type::Greater => {
                self.add_instruction_from(Instruction::Greater, line);
            }
            Type::GreaterEqual => {
                self.add_instruction_from(Instruction::Less, line);
                self.add_instruction_from(Instruction::Not, line);
            }
            Type::Less => {
                self.add_instruction_from(Instruction::Less, line);
            }
            Type::LessEqual => {
                self.add_instruction_from(Instruction::Greater, line);
                self.add_instruction_from(Instruction::Not, line);
            }
            _ => panic!("Unreachable"),
        }

        Ok(())
    }

    fn call(&mut self, _: bool) -> CompilerResult<()> {
        let args = self.argument_list()?;
        if args.count > 255 {
            return Err(self.error("Can't have more than 255 parameters."));
        }

        self.add_instruction(Instruction::Call(args));
        Ok(())
    }

    // When this is called, the left-hand side expression has already been compiled. If that value
    // is false, leave that value on the stack and skip the whole expression. If not, pop that
    // value from the stack and evaluate the next expression.
    fn and(&mut self, _: bool) -> CompilerResult<()> {
        let end_jump = self.add_instruction(Instruction::Jump(JumpDist { dist: 0 }));
        self.add_instruction(Instruction::Pop);
        self.parse_precedence(Precedence::And)?;
        self.fun.chunk.patch_jump(end_jump);
        Ok(())
    }

    // For else, if the value is false, pop the value and continue with the next expression (here
    // the next expression starts after the unconditional jump that skips to the next instruction
    // after the or expression which is taken when the value is true).
    fn or(&mut self, _: bool) -> CompilerResult<()> {
        let line = self.prev.as_ref().unwrap().line;
        let else_jump =
            self.add_instruction_from(Instruction::JumpIfFalse(JumpDist { dist: 0 }), line);
        let end_jump = self.add_instruction_from(Instruction::Jump(JumpDist { dist: 0 }), line);
        self.fun.chunk.patch_jump(else_jump);
        self.add_instruction_from(Instruction::Pop, line);
        self.parse_precedence(Precedence::Or)?;
        self.fun.chunk.patch_jump(end_jump);
        Ok(())
    }

    fn literal(&mut self, _: bool) -> CompilerResult<()> {
        let prev = self.prev.as_ref().unwrap();
        match prev.ty {
            Type::True => {
                self.add_instruction_from(Instruction::LiteralTrue, prev.line);
            }
            Type::False => {
                self.add_instruction_from(Instruction::LiteralFalse, prev.line);
            }
            Type::Nil => {
                self.add_instruction_from(Instruction::LiteralNil, prev.line);
            }
            _ => panic!("Unreachable"),
        }

        Ok(())
    }

    fn number(&mut self, _: bool) -> CompilerResult<()> {
        let prev = self.prev.as_ref().unwrap();
        let value = Value::from(prev.value.clone());
        let constant = self.fun.chunk.make_constant(value).unwrap();
        self.add_instruction_from(Instruction::Constant(constant), prev.line);
        Ok(())
    }

    fn string(&mut self, _: bool) -> CompilerResult<()> {
        let prev = self.prev.as_ref().unwrap();
        let value = Value::from(prev.value.clone());
        let constant = self.fun.chunk.make_constant(value).unwrap();
        self.add_instruction_from(Instruction::Constant(constant), prev.line);
        Ok(())
    }

    fn variable(&mut self, can_assign: bool) -> CompilerResult<()> {
        self.named_variable(can_assign)
    }

    fn named_variable(&mut self, can_assign: bool) -> CompilerResult<()> {
        let prev = self.prev.as_ref().unwrap();

        let name = prev.lexeme.clone();
        let line = prev.line;
        let (set_op, get_op) = match self.resolve_local(&name) {
            // The local variable cannot be resolved locally, so this variable must be a global
            // variable.
            Ok(-1) => {
                let constant = self.identifier_constant(name);
                (
                    Instruction::SetGlobal(constant),
                    Instruction::GetGlobal(constant),
                )
            }
            Ok(arg) => (
                Instruction::SetLocal(StackOffset { index: arg as u8 }),
                Instruction::GetLocal(StackOffset { index: arg as u8 }),
            ),
            Err(err) => {
                return Err(err);
            }
        };

        if can_assign && self.match_type(Type::Equal) {
            self.expression()?;
            self.add_instruction_from(set_op, line);
        } else {
            self.add_instruction_from(get_op, line);
        }
        Ok(())
    }

    // This will parse an expression with precedence higher than the given one starting at the
    // current token.
    // The first token will always belong to some kind of prefix expression, otherwise it's a syntax
    // error. (Think about how this method is called in `expression()`).
    // After parsing with prefix function, check if next token's infix operation precedence is
    // higher than the current precedence defined. As long as the next token's infix precedence
    // is higher, the parser will keep parsing those expression until none is left.
    fn parse_precedence(&mut self, precedence: Precedence) -> CompilerResult<()> {
        self.advance();
        let prev = self.prev.as_ref().unwrap();
        let rule = &Self::PARSE_RULES[prev.ty as usize];

        // This will prevent the compiler from parsing unassign-able expressions.
        // Say you're parsing "a * b = c + d".
        // During the second call to `variable()` (an infix rule that stems from `b`) after
        // parsing "*", the assignment will be parsed in the same function and the parsed result
        // will be wrong.
        // To avoid that, if the current precedence is greater than assignment (in the example,
        // "*"'s precedence is higher so this will be false), the assignability is passed into
        // prefix function. The variable parsing function will see this and instead of continuing
        // the consumption of "=", it will stop and produce `GetGlobal` instruction instead.
        // This means that there will be an equal sign left to be consumed afterwards if the current
        // precedence is lower than assignment (e.g literals). In that case, the program is trying
        // to assign a non-assignable expression and this should be notified to the user.
        let can_assign = precedence as usize <= Precedence::Assignment as usize;

        match rule.prefix() {
            Some(prefix) => prefix(self, can_assign)?,
            None => {
                return Err(self.error("Expect expression."));
            }
        }

        while precedence as usize
            <= *Self::PARSE_RULES[self.curr.as_ref().unwrap().ty as usize].precedence() as usize
        {
            self.advance();
            let rule = &Self::PARSE_RULES[self.prev.as_ref().unwrap().ty as usize];
            (rule.infix().unwrap())(self, false)?;
        }

        if can_assign && self.match_type(Type::Equal) {
            Err(self.error("Invalid assignment target."))
        } else {
            Ok(())
        }
    }

    fn parse_variable(&mut self, msg: &str) -> CompilerResult<Constant> {
        self.consume(Type::Identifier, msg)?;
        let prev = self.prev.as_ref().unwrap();
        let name = prev.lexeme.clone();

        self.declare_variable()?;
        Ok(self.identifier_constant(name))
    }

    fn declare_variable(&mut self) -> CompilerResult<()> {
        if self.scope_depth == 0 {
            return Ok(());
        }

        let name = self.prev.as_ref().unwrap().lexeme.clone();
        for local in self.locals.iter().rev() {
            // Depth = -1 means the variable has been declared but not defined. Only check the locals
            // from the current scope.
            if local.depth != -1 && local.depth < self.scope_depth {
                break;
            }

            if local.name == name {
                return Err(self.error("Already a variable with this name in this scope."));
            }
        }

        self.add_local(name)?;
        Ok(())
    }

    fn add_local(&mut self, name: String) -> CompilerResult<()> {
        if self.locals.len() == Self::STACK_SIZE {
            Err(self.error("Too many local variables in function."))
        } else {
            self.locals.push(Local { name, depth: -1 });
            Ok(())
        }
    }

    fn resolve_local(&mut self, name: &str) -> CompilerResult<isize> {
        for (idx, local) in self.locals.iter().rev().enumerate() {
            if local.name == name {
                return if local.depth == -1 {
                    Err(self.error("Can't read local variable in its own initializer"))
                } else {
                    Ok(idx as isize)
                };
            }
        }

        Ok(-1)
    }

    fn identifier_constant(&mut self, name: String) -> Constant {
        self.fun.chunk.make_constant(Value::Str(name)).unwrap()
    }

    fn define_variable(&mut self, constant: Constant, line: usize) {
        // For local scope, the variable doesn't even need to be defined. They can be referred
        // by scope index.
        if self.scope_depth > 0 {
            self.mark_initialized();
            return;
        }

        self.add_instruction_from(Instruction::DefineGlobal(constant), line);
    }

    fn argument_list(&mut self) -> CompilerResult<ArgCount> {
        let mut count = 0;
        if !self.check(Type::RightParen) {
            loop {
                self.expression()?;
                count += 1;

                if !self.match_type(Type::Comma) {
                    break;
                }
            }
        }

        self.consume(Type::RightParen, "Expect ')' after arguments.")?;
        Ok(ArgCount { count })
    }

    fn mark_initialized(&mut self) {
        if self.scope_depth == 0 {
            return;
        }

        self.locals.last_mut().unwrap().depth = self.scope_depth;
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
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

    fn add_return(&mut self) {
        self.add_instruction(Instruction::LiteralNil);
        self.add_instruction(Instruction::Return);
    }

    fn add_instruction(&mut self, instruction: Instruction) -> usize {
        self.add_instruction_from(instruction, self.prev.as_ref().unwrap().line)
    }

    fn add_instruction_from(&mut self, instruction: Instruction, from_line: usize) -> usize {
        self.fun.chunk.add_instruction(instruction, from_line)
    }

    fn advance(&mut self) {
        self.prev = self.curr.take();
        self.curr = self.stream.next();
    }

    // Consume will return the line number of the consumed token for ease.
    // Maybe I should return the reference to the token as a result instead of just the line number.
    // This way, I can bubble up the result back to the main compile function too.
    fn consume(&mut self, ty: Type, msg: &str) -> CompilerResult<usize> {
        match &self.curr {
            Some(token) if token.ty == ty => {
                let line = token.line;
                self.advance();
                Ok(line)
            }
            _ => Err(self.error(msg)),
        }
    }

    fn match_type(&mut self, ty: Type) -> bool {
        if !self.check(ty) {
            false
        } else {
            self.advance();
            true
        }
    }

    fn emit_loop(&mut self, start: usize, line: usize) {
        let dist = self.fun.chunk.in_count() - start;
        self.add_instruction_from(Instruction::Loop(JumpDist { dist }), line);
    }

    fn check(&self, ty: Type) -> bool {
        self.curr.as_ref().unwrap().ty == ty
    }

    fn error(&mut self, msg: &str) -> Error {
        let line = if let Some(curr) = &self.curr {
            curr.line
        } else {
            0
        };

        Error::compile(line, msg)
    }

    // Synchronize the token stream if an error is found during compilation, and the course of action
    // is to simply skip all tokens until the end of statement or the eof is found.
    fn synchronize(&mut self) {
        while self.curr.as_ref().unwrap().ty != Type::Eof {
            if let Type::SemiColon = self.prev.as_ref().unwrap().ty {
                return;
            }

            match self.curr.as_ref().unwrap().ty {
                Type::Class
                | Type::Fun
                | Type::Var
                | Type::For
                | Type::If
                | Type::While
                | Type::Print
                | Type::Return => {
                    return;
                }
                _ => self.advance(),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::compiler::Compiler;
    use losk_core::*;

    #[test]
    fn test_compilation() {
        let mut scanner = Scanner::new();
        let stream = scanner.scan_tokens("(-1 + 2) * 3 - -4");
        let compiler = Compiler::new();
        let res = compiler.compile(stream);
    }
}
