use crate::chunk::{Chunk, Instruction};
use crate::error::{CompilerResult, Error};
use crate::instruction::{Constant, JumpDist, StackOffset};
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

#[allow(dead_code)]
impl Compiler {
    pub(crate) fn new() -> Self {
        Compiler
    }

    pub(crate) fn compile(&self, stream: TokenStream) -> Result<Chunk, Vec<Error>> {
        let ctx = Context::compiled(stream);

        if !ctx.errs.is_empty() {
            Err(ctx.errs)
        } else {
            Ok(ctx.chunk)
        }
    }
}

struct Local {
    name: String,
    depth: isize,
}

struct Context<'a> {
    stream: TokenStream<'a>,
    chunk: Chunk,

    curr: Option<Token>,
    prev: Option<Token>,

    errs: Vec<Error>,
    panic: bool,

    // locals are used to resolve the declared variables into a stack frame location. `scope_depth`
    // is an auxiliary data that is used in this task, by tracking which level of scope the compiler
    // is currently in.
    // The textbook uses a pointer to token for local's name, but in this implementation, the tokens
    // are immediately discarded after usage and I can't keep their pointers. So their names will
    // be copied instead.
    locals: Vec<Local>,
    scope_depth: isize,
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
        ParseRule(Some(Self::grouping), None, Precedence::None), // LeftParen
        ParseRule(None, None, Precedence::None),                 // RightParen
        ParseRule(None, None, Precedence::None),                 // LeftBrace
        ParseRule(None, None, Precedence::None),                 // RightBrace
        ParseRule(None, None, Precedence::None),                 // Comma
        ParseRule(None, None, Precedence::None),                 // Dot
        ParseRule(Some(Self::unary), Some(Self::binary), Precedence::Term), // Minus
        ParseRule(None, Some(Self::binary), Precedence::Term),   // Plus
        ParseRule(None, None, Precedence::None),                 // Semicolon
        ParseRule(None, Some(Self::binary), Precedence::Factor), // Slash
        ParseRule(None, Some(Self::binary), Precedence::Factor), // Star
        ParseRule(None, None, Precedence::None),                 // Bang
        ParseRule(None, Some(Self::binary), Precedence::Equality), // BangEqual
        ParseRule(None, None, Precedence::None),                 // Equal
        ParseRule(None, Some(Self::binary), Precedence::Equality), // EqualEqual
        ParseRule(None, Some(Self::binary), Precedence::Comparison), // Greater
        ParseRule(None, Some(Self::binary), Precedence::Comparison), // GreaterEqual
        ParseRule(None, Some(Self::binary), Precedence::Comparison), // Less
        ParseRule(None, Some(Self::binary), Precedence::Comparison), // LessEqual
        ParseRule(Some(Self::variable), None, Precedence::None), // Identifier
        ParseRule(Some(Self::string), None, Precedence::None),   // String
        ParseRule(Some(Self::number), None, Precedence::None),   // Number
        ParseRule(None, Some(Self::and), Precedence::And),       // And
        ParseRule(None, None, Precedence::None),                 // Class
        ParseRule(None, None, Precedence::None),                 // Else
        ParseRule(Some(Self::literal), None, Precedence::None),  // True
        ParseRule(Some(Self::literal), None, Precedence::None),  // False
        ParseRule(None, None, Precedence::None),                 // For
        ParseRule(None, None, Precedence::None),                 // Fun
        ParseRule(None, None, Precedence::None),                 // If
        ParseRule(Some(Self::literal), None, Precedence::None),  // Nil
        ParseRule(None, Some(Self::or), Precedence::Or),         // Or
        ParseRule(None, None, Precedence::None),                 // Print
        ParseRule(None, None, Precedence::None),                 // Return
        ParseRule(None, None, Precedence::None),                 // Super
        ParseRule(None, None, Precedence::None),                 // This
        ParseRule(None, None, Precedence::None),                 // Var
        ParseRule(None, None, Precedence::None),                 // While
        ParseRule(None, None, Precedence::None),                 // Error
    ];

    // Instead of having this function inside the context, this context should be passed mutably through
    // compiler's methods. This would make some of the multiple borrows restriction easier.
    fn compiled(stream: TokenStream<'a>) -> Self {
        let mut ctx = Context {
            stream,
            chunk: Chunk::new(),

            curr: None,
            prev: None,

            errs: Vec::new(),
            panic: false,

            locals: Vec::new(),
            scope_depth: 0,
        };

        ctx.compile();
        ctx
    }

    fn compile(&mut self) {
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
    }

    fn declaration(&mut self) -> CompilerResult<()> {
        if self.match_type(Type::Var) {
            self.var_declaration()
        } else {
            self.statement()
        }
    }

    fn var_declaration(&mut self) -> CompilerResult<()> {
        let constant = self.parse_variable("Expect variable name.")?;

        if self.match_type(Type::Equal) {
            self.expression()?;
        } else {
            self.chunk
                .add_instruction(Instruction::LiteralNil, self.prev.as_ref().unwrap().line);
        }

        let line = self.consume(Type::SemiColon, "Expect ';' after variable declaration.")?;
        self.define_variable(constant, line);
        Ok(())
    }

    fn statement(&mut self) -> CompilerResult<()> {
        if self.match_type(Type::Print) {
            self.print_statement()?;
        } else if self.match_type(Type::If) {
            self.if_statement()?;
        } else if self.match_type(Type::While) {
            self.while_statement()?;
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
        self.chunk.add_instruction(Instruction::Print, line);
        Ok(())
    }

    fn if_statement(&mut self) -> CompilerResult<()> {
        self.consume(Type::LeftParen, "Expect '(' after 'if'.")?;
        self.expression()?;
        let mut line = self.consume(Type::RightParen, "Expect ')' after condition.")?;

        let then_jump = self
            .chunk
            .add_instruction(Instruction::JumpIfFalse(JumpDist { dist: 0 }), line);
        self.chunk.add_instruction(Instruction::Pop, line);
        self.statement()?;

        line = self.prev.as_ref().unwrap().line;
        let else_jump = self
            .chunk
            .add_instruction(Instruction::Jump(JumpDist { dist: 0 }), line);

        // Patching jump, here the if statement has been parsed and the program will jump to this
        // place if the test condition is false. In short,
        // then_jump = jump over the "then" statements after "if" when the condition is false
        // else_jump = jump over the "else" statements after executing if statements
        self.chunk.patch_jump(then_jump);
        self.chunk.add_instruction(Instruction::Pop, line);

        if self.match_type(Type::Else) {
            self.statement()?;
        }

        // Need another jump patching here because without this patch, the if statement would just
        // fall through into else clause.
        self.chunk.patch_jump(else_jump);
        Ok(())
    }

    fn while_statement(&mut self) -> CompilerResult<()> {
        let loop_start = self.chunk.in_count();
        self.consume(Type::LeftParen, "Expect '(' after while.")?;
        self.expression()?;
        self.consume(Type::RightParen, "Expect ')' after condition.")?;

        let mut line = self.prev.as_ref().unwrap().line;
        let exit_jump = self
            .chunk
            .add_instruction(Instruction::JumpIfFalse(JumpDist { dist: 0 }), line);
        self.chunk.add_instruction(Instruction::Pop, line);
        self.statement()?;

        line = self.prev.as_ref().unwrap().line;
        self.emit_loop(loop_start, line);
        self.chunk.patch_jump(exit_jump);
        line = self.prev.as_ref().unwrap().line;
        self.chunk.add_instruction(Instruction::Pop, line);

        Ok(())
    }

    fn expression_statement(&mut self) -> CompilerResult<()> {
        self.expression()?;
        let line = self.consume(Type::SemiColon, "Expect ';' after expression.")?;
        self.chunk.add_instruction(Instruction::Pop, line);
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
                self.chunk.add_instruction(Instruction::Negate, line);
            }
            Type::Bang => {
                self.chunk.add_instruction(Instruction::Not, line);
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
                self.chunk.add_instruction(Instruction::Add, line);
            }
            Type::Minus => {
                self.chunk.add_instruction(Instruction::Subtract, line);
            }
            Type::Star => {
                self.chunk.add_instruction(Instruction::Multiply, line);
            }
            Type::Slash => {
                self.chunk.add_instruction(Instruction::Divide, line);
            }
            Type::BangEqual => {
                self.chunk.add_instruction(Instruction::Equal, line);
                self.chunk.add_instruction(Instruction::Not, line);
            }
            Type::EqualEqual => {
                self.chunk.add_instruction(Instruction::Equal, line);
            }
            Type::Greater => {
                self.chunk.add_instruction(Instruction::Greater, line);
            }
            Type::GreaterEqual => {
                self.chunk.add_instruction(Instruction::Less, line);
                self.chunk.add_instruction(Instruction::Not, line);
            }
            Type::Less => {
                self.chunk.add_instruction(Instruction::Less, line);
            }
            Type::LessEqual => {
                self.chunk.add_instruction(Instruction::Greater, line);
                self.chunk.add_instruction(Instruction::Not, line);
            }
            _ => panic!("Unreachable"),
        }

        Ok(())
    }

    // When this is called, the left-hand side expression has already been compiled. If that value
    // is false, leave that value on the stack and skip the whole expression. If not, pop that
    // value from the stack and evaluate the next expression.
    fn and(&mut self, _: bool) -> CompilerResult<()> {
        let line = self.prev.as_ref().unwrap().line;
        let end_jump = self
            .chunk
            .add_instruction(Instruction::Jump(JumpDist { dist: 0 }), line);
        self.chunk.add_instruction(Instruction::Pop, line);
        self.parse_precedence(Precedence::And)?;
        self.chunk.patch_jump(end_jump);
        Ok(())
    }

    // For else, if the value is false, pop the value and continue with the next expression (here
    // the next expression starts after the unconditional jump that skips to the next instruction
    // after the or expression which is taken when the value is true).
    fn or(&mut self, _: bool) -> CompilerResult<()> {
        let line = self.prev.as_ref().unwrap().line;
        let else_jump = self
            .chunk
            .add_instruction(Instruction::JumpIfFalse(JumpDist { dist: 0 }), line);
        let end_jump = self
            .chunk
            .add_instruction(Instruction::Jump(JumpDist { dist: 0 }), line);
        self.chunk.patch_jump(else_jump);
        self.chunk.add_instruction(Instruction::Pop, line);
        self.parse_precedence(Precedence::Or)?;
        self.chunk.patch_jump(end_jump);
        Ok(())
    }

    fn literal(&mut self, _: bool) -> CompilerResult<()> {
        let prev = self.prev.as_ref().unwrap();
        match prev.ty {
            Type::True => {
                self.chunk
                    .add_instruction(Instruction::LiteralTrue, prev.line);
            }
            Type::False => {
                self.chunk
                    .add_instruction(Instruction::LiteralFalse, prev.line);
            }
            Type::Nil => {
                self.chunk
                    .add_instruction(Instruction::LiteralNil, prev.line);
            }
            _ => panic!("Unreachable"),
        }

        Ok(())
    }

    fn number(&mut self, _: bool) -> CompilerResult<()> {
        let prev = self.prev.as_ref().unwrap();
        let value = Value::from(prev.value.clone());
        let constant = self.chunk.make_constant(value).unwrap();
        self.chunk
            .add_instruction(Instruction::Constant(constant), prev.line);
        Ok(())
    }

    fn string(&mut self, _: bool) -> CompilerResult<()> {
        let prev = self.prev.as_ref().unwrap();
        let value = Value::from(prev.value.clone());
        let constant = self.chunk.make_constant(value).unwrap();
        self.chunk
            .add_instruction(Instruction::Constant(constant), prev.line);
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
            self.chunk.add_instruction(set_op, line);
        } else {
            self.chunk.add_instruction(get_op, line);
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
        self.chunk.make_constant(Value::Str(name)).unwrap()
    }

    fn define_variable(&mut self, constant: Constant, line: usize) {
        // For local scope, the variable doesn't even need to be defined. They can be referred
        // by scope index.
        if self.scope_depth > 0 {
            self.mark_initialized();
            return;
        }

        self.chunk
            .add_instruction(Instruction::DefineGlobal(constant), line);
    }

    fn mark_initialized(&mut self) {
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
            let line = self.prev.as_ref().unwrap().line;
            self.chunk.add_instruction(Instruction::PopN(n), line);
        }
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
        let dist = self.chunk.in_count() - start;
        self.chunk
            .add_instruction(Instruction::Loop(JumpDist { dist }), line);
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
        self.panic = false;

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
                _ => {}
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
