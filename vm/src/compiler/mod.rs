use thiserror::Error;

use losk_core::{TokenStream, Type};

use crate::chunk::*;
use crate::compiler::context::*;
use crate::limits::COMP_ARG_LIMIT;
use crate::value::ConstantValue;
use crate::Function;

mod context;

struct LocalIndex(usize);

enum LocalResolution {
    Local(LocalIndex),
    UpValue(UpvalueIndex),
    Global,
}

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
    Method,
    Initializer,
}

struct Local {
    name: String,
    depth: isize,
    is_captured: bool,
}

pub struct Compiler;

type CompilationResult<T> = Result<T, CompileError>;
type ParseFn = fn(&Compiler, &mut Context, bool) -> CompilationResult<()>;

struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence,
}

impl ParseRule {
    fn new(prefix: Option<ParseFn>, infix: Option<ParseFn>, precedence: Precedence) -> Self {
        ParseRule {
            prefix,
            infix,
            precedence,
        }
    }
}

#[derive(Debug, Error, PartialEq)]
#[error("[line {line:?}] compile error: {msg}")]
pub struct CompileError {
    line: usize,
    msg: String,
}

impl CompileError {
    fn new(line: usize, msg: &str) -> CompileError {
        CompileError {
            line,
            msg: msg.to_string(),
        }
    }
}

impl Compiler {
    const STACK_SIZE: usize = u8::MAX as usize + 1;

    // A Pratt parser's table translated as a match. The first column
    // maps a token type to prefix parsing function while the second column maps to infix parsing
    // function. The third column precedence represents the precedence of the infix expression
    // which uses that token as an operator.
    //
    // Precedence of the infix operators don't need to be tracked since the precedence of all prefix
    // operators are the same.
    fn rule(ty: Type) -> ParseRule {
        match ty {
            Type::LeftParen => {
                ParseRule::new(Some(Self::grouping), Some(Self::call), Precedence::Call)
            }
            Type::RightParen => ParseRule::new(None, None, Precedence::None),
            Type::LeftBrace => ParseRule::new(None, None, Precedence::None),
            Type::RightBrace => ParseRule::new(None, None, Precedence::None),
            Type::Comma => ParseRule::new(None, None, Precedence::None),
            Type::Dot => ParseRule::new(None, Some(Self::dot), Precedence::Call),
            Type::Minus => ParseRule::new(Some(Self::unary), Some(Self::binary), Precedence::Term),
            Type::Plus => ParseRule::new(None, Some(Self::binary), Precedence::Term),
            Type::SemiColon => ParseRule::new(None, None, Precedence::None),
            Type::Slash => ParseRule::new(None, Some(Self::binary), Precedence::Factor),
            Type::Star => ParseRule::new(None, Some(Self::binary), Precedence::Factor),
            Type::Bang => ParseRule::new(Some(Self::unary), None, Precedence::None),
            Type::BangEqual => ParseRule::new(None, Some(Self::binary), Precedence::Equality),
            Type::Equal => ParseRule::new(None, None, Precedence::None),
            Type::EqualEqual => ParseRule::new(None, Some(Self::binary), Precedence::Equality),
            Type::Greater => ParseRule::new(None, Some(Self::binary), Precedence::Comparison),
            Type::GreaterEqual => ParseRule::new(None, Some(Self::binary), Precedence::Comparison),
            Type::Less => ParseRule::new(None, Some(Self::binary), Precedence::Comparison),
            Type::LessEqual => ParseRule::new(None, Some(Self::binary), Precedence::Comparison),
            Type::Identifier => ParseRule::new(Some(Self::variable), None, Precedence::None),
            Type::String => ParseRule::new(Some(Self::string), None, Precedence::None),
            Type::Number => ParseRule::new(Some(Self::number), None, Precedence::None),
            Type::And => ParseRule::new(None, Some(Self::and), Precedence::And),
            Type::Class => ParseRule::new(None, None, Precedence::None),
            Type::Else => ParseRule::new(None, None, Precedence::None),
            Type::True => ParseRule::new(Some(Self::literal), None, Precedence::None),
            Type::False => ParseRule::new(Some(Self::literal), None, Precedence::None),
            Type::For => ParseRule::new(None, None, Precedence::None),
            Type::Fun => ParseRule::new(None, None, Precedence::None),
            Type::If => ParseRule::new(None, None, Precedence::None),
            Type::Nil => ParseRule::new(Some(Self::literal), None, Precedence::None),
            Type::Or => ParseRule::new(None, Some(Self::or), Precedence::Or),
            Type::Print => ParseRule::new(None, None, Precedence::None),
            Type::Return => ParseRule::new(None, None, Precedence::None),
            Type::Super => ParseRule::new(Some(Self::super_), None, Precedence::None),
            Type::This => ParseRule::new(Some(Self::this), None, Precedence::None),
            Type::Var => ParseRule::new(None, None, Precedence::None),
            Type::While => ParseRule::new(None, None, Precedence::None),
            Type::Eof => ParseRule::new(None, None, Precedence::None),
        }
    }

    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Compiler
    }

    pub fn compile(&self, stream: TokenStream) -> Result<Function, Vec<CompileError>> {
        let mut ctx = Context::new(stream, FunctionType::Script, None);
        self.compile_context(&mut ctx);

        if !ctx.errs.is_empty() {
            Err(ctx.errs)
        } else {
            Ok(ctx.fun)
        }
    }

    fn compile_context(&self, ctx: &mut Context) {
        // A dummy local value needs to be added for special local zero slot that will be used for
        // the closure
        match ctx.ftype {
            FunctionType::Method | FunctionType::Initializer => {
                ctx.add_local("this".to_string(), Some(0)).unwrap()
            }
            FunctionType::Function | FunctionType::Script => {
                ctx.add_local(String::new(), Some(0)).unwrap()
            }
        };

        // If this is a function, the compiler doesn't need to parse until EOF. It's enough to
        // parse just the function name, parameters, and the body
        //
        // The return functions are always emitted whether the parsed function does return or
        // not. They will not be interpreted if the function return earlier anyway.
        match ctx.ftype {
            FunctionType::Function | FunctionType::Method | FunctionType::Initializer => {
                match self.compile_function(ctx) {
                    Ok(_) => ctx.add_return(),
                    Err(err) => {
                        ctx.errs.push(err);

                        // The synchronize here should be synchronized until the end of the block?
                        self.synchronize(ctx);
                    }
                }

                return;
            }
            FunctionType::Script => { /* continue */ }
        }

        // Only advance to next character if the function being parsed is a script because
        // if function, the context needs access to previous token for naming the function.
        ctx.advance();

        // Maybe check the result at this level and do the synchronisation here?
        while !ctx.match_type(Type::Eof) {
            match self.declaration(ctx) {
                Ok(_) => {}
                Err(err) => {
                    ctx.errs.push(err);
                    self.synchronize(ctx);
                }
            }
        }

        ctx.add_return();
    }

    fn compile_function(&self, ctx: &mut Context) -> CompilationResult<()> {
        ctx.fun.name = ctx.prev.as_ref().unwrap().lexeme.clone();

        ctx.begin_scope();
        ctx.consume(Type::LeftParen, "Expect '(' after function name.")?;
        if !ctx.check(Type::RightParen) {
            loop {
                ctx.fun.arity += 1;
                if ctx.fun.arity > 255 {
                    return Err(ctx.error("Can't have more than 255 parameters."));
                }

                let constant = self.parse_variable(ctx, "Expect parameter name")?;
                ctx.define_variable(constant, ctx.prev.as_ref().unwrap().line);

                if !ctx.match_type(Type::Comma) {
                    break;
                }
            }
        }

        ctx.consume(Type::RightParen, "Expect ')' after function parameters.")?;
        ctx.consume(Type::LeftBrace, "Expect '{' before function boyd.")?;
        self.block(ctx)?;
        Ok(())
    }

    fn declaration(&self, ctx: &mut Context) -> CompilationResult<()> {
        if ctx.match_type(Type::Var) {
            self.var_declaration(ctx)
        } else if ctx.match_type(Type::Fun) {
            self.fun_declaration(ctx)
        } else if ctx.match_type(Type::Class) {
            self.class_declaration(ctx)
        } else {
            self.statement(ctx)
        }
    }

    fn var_declaration(&self, ctx: &mut Context) -> CompilationResult<()> {
        let constant = self.parse_variable(ctx, "Expect variable name.")?;

        if ctx.match_type(Type::Equal) {
            self.expression(ctx)?;
        } else {
            ctx.add_instruction(Instruction::LiteralNil);
        }

        let line = ctx.consume(Type::SemiColon, "Expect ';' after variable declaration.")?;
        ctx.define_variable(constant, line);
        Ok(())
    }

    fn fun_declaration(&self, ctx: &mut Context) -> CompilationResult<()> {
        let global = self.parse_variable(ctx, "Expect function name")?;
        ctx.mark_initialized();
        self.function(ctx, FunctionType::Function)?;
        ctx.define_variable(global, ctx.prev.as_ref().unwrap().line);
        Ok(())
    }

    fn class_declaration(&self, ctx: &mut Context) -> CompilationResult<()> {
        let prev_has_superclass = ctx.has_superclass;
        let prev_is_in_class = ctx.is_in_class;
        ctx.is_in_class = true;

        let line = ctx.consume(Type::Identifier, "Expect class name.")?;
        let name = ctx.prev.as_ref().unwrap().lexeme.clone();
        let name_constant = ctx.identifier_constant(name.clone())?;

        self.declare_variable(ctx)?;

        ctx.add_instruction(Instruction::Class(name_constant));
        ctx.define_variable(name_constant, line);

        if ctx.match_type(Type::Less) {
            ctx.consume(Type::Identifier, "Expect superclass name")?;
            let superclass = ctx.prev.as_ref().unwrap();
            if name == superclass.lexeme {
                return Err(ctx.error("A class can't inherit from itself."));
            }
            self.variable(ctx, false)?;

            ctx.begin_scope();
            ctx.add_local("super".to_string(), None)?;
            ctx.define_variable(Constant(0), line);

            self.named_variable(ctx, false, name.clone(), line)?;
            ctx.add_instruction(Instruction::Inherit);
            ctx.has_superclass = true;
        }

        self.named_variable(ctx, false, name, line)?;
        ctx.consume(Type::LeftBrace, "Expect '{' before class body.")?;
        while !ctx.check(Type::RightBrace) && !ctx.check(Type::Eof) {
            self.method(ctx)?;
        }
        ctx.consume(Type::RightBrace, "Expect '}' after class body.")?;

        // After building all the methods, the class variable is still on top of the stack and it
        // needs to be popped.
        ctx.add_instruction(Instruction::Pop);

        if ctx.has_superclass {
            ctx.end_scope();
        }

        ctx.has_superclass = prev_has_superclass;
        ctx.is_in_class = prev_is_in_class;
        Ok(())
    }

    fn statement(&self, ctx: &mut Context) -> CompilationResult<()> {
        if ctx.match_type(Type::Print) {
            self.print_statement(ctx)?;
        } else if ctx.match_type(Type::If) {
            self.if_statement(ctx)?;
        } else if ctx.match_type(Type::Return) {
            self.return_statement(ctx)?;
        } else if ctx.match_type(Type::While) {
            self.while_statement(ctx)?;
        } else if ctx.match_type(Type::For) {
            self.for_statement(ctx)?;
        } else if ctx.match_type(Type::LeftBrace) {
            ctx.begin_scope();
            self.block(ctx)?;
            ctx.end_scope();
        } else {
            self.expression_statement(ctx)?;
        }

        Ok(())
    }

    fn print_statement(&self, ctx: &mut Context) -> CompilationResult<()> {
        self.expression(ctx)?;
        let line = ctx.consume(Type::SemiColon, "Expect ';' after value.")?;
        ctx.add_instruction_from(Instruction::Print, line);
        Ok(())
    }

    fn return_statement(&self, ctx: &mut Context) -> CompilationResult<()> {
        if ctx.ftype == FunctionType::Script {
            return Err(ctx.error("Can't return from top-level code."));
        }

        if ctx.match_type(Type::SemiColon) {
            ctx.add_return();
        } else {
            if let FunctionType::Initializer = ctx.ftype {
                return Err(ctx.error("Can't return a value from an initializer."));
            }

            self.expression(ctx)?;
            ctx.consume(Type::SemiColon, "Expect ';' after return value.")?;
            ctx.add_instruction(Instruction::Return);
        }

        Ok(())
    }

    fn if_statement(&self, ctx: &mut Context) -> CompilationResult<()> {
        ctx.consume(Type::LeftParen, "Expect '(' after 'if'.")?;
        self.expression(ctx)?;

        let mut line = ctx.consume(Type::RightParen, "Expect ')' after condition.")?;
        let then_jump = ctx.add_instruction_from(Instruction::JumpIfFalse(JumpDist(0)), line);
        ctx.add_instruction_from(Instruction::Pop, line);
        self.statement(ctx)?;

        line = ctx.prev.as_ref().unwrap().line;
        let else_jump = ctx.add_instruction_from(Instruction::Jump(JumpDist(0)), line);

        // Patching jump, here the if statement has been parsed and the program will jump to this
        // place if the test condition is false. In short,
        // then_jump = jump over the "then" statements after "if" when the condition is false
        // else_jump = jump over the "else" statements after executing if statements
        ctx.fun.chunk.patch_jump(then_jump);
        ctx.add_instruction_from(Instruction::Pop, line);

        if ctx.match_type(Type::Else) {
            self.statement(ctx)?;
        }

        // Need another jump patching here because without this patch, the if statement would just
        // fall through into else clause.
        ctx.fun.chunk.patch_jump(else_jump);
        Ok(())
    }

    fn while_statement(&self, ctx: &mut Context) -> CompilationResult<()> {
        let loop_start = ctx.fun.chunk.in_count();
        ctx.consume(Type::LeftParen, "Expect '(' after while.")?;
        self.expression(ctx)?;

        let mut line = ctx.consume(Type::RightParen, "Expect ')' after condition.")?;
        let exit_jump = ctx.add_instruction_from(Instruction::JumpIfFalse(JumpDist(0)), line);
        ctx.add_instruction_from(Instruction::Pop, line);
        self.statement(ctx)?;

        line = ctx.prev.as_ref().unwrap().line;
        ctx.emit_loop(loop_start, line)?;
        ctx.fun.chunk.patch_jump(exit_jump);
        ctx.add_instruction(Instruction::Pop);

        Ok(())
    }

    fn for_statement(&self, ctx: &mut Context) -> CompilationResult<()> {
        // The for variable is scoped to the body, so a new scope is created.
        ctx.begin_scope();
        ctx.consume(Type::LeftParen, "Expect '(' after 'for'.")?;

        // The first part, initialization statement.
        if ctx.match_type(Type::SemiColon) {
            // No initializer
        } else if ctx.match_type(Type::Var) {
            self.var_declaration(ctx)?;
        } else {
            self.expression_statement(ctx)?;
        }

        // The second part, where the condition is tested before running the block.
        let mut loop_start = ctx.fun.chunk.in_count();
        let mut exit_jump = None;
        if !ctx.match_type(Type::SemiColon) {
            self.expression(ctx)?;
            let line = ctx.consume(Type::SemiColon, "Expect ';' after loop condition.")?;

            // This jump will be patched to skip the block if the condition is false.
            exit_jump = Some(ctx.add_instruction_from(Instruction::JumpIfFalse(JumpDist(0)), line));
            ctx.add_instruction_from(Instruction::Pop, line); // pop the condition
        }

        // The third part, incrementing the variable (or any expression statement - without ";").
        // This will be run after every block.
        if !ctx.match_type(Type::RightParen) {
            let body_jump = ctx.add_instruction(Instruction::Jump(JumpDist(0)));
            let increment_start = ctx.fun.chunk.in_count();
            self.expression(ctx)?;
            ctx.add_instruction(Instruction::Pop);

            // After incrementing the variable, loop back to the start - which is the condition
            // testing instruction.
            let line = ctx.consume(Type::RightParen, "Expect ')' after for clauses.")?;
            ctx.emit_loop(loop_start, line)?;

            // The block is set to be looped back to the start - which normally is testing condition.
            // But if there's incrementing statement, it should be ran first before test condition.
            // It's achieved here by overriding the loop start index.
            loop_start = increment_start;

            // `body_jump` was emitted before the increment part, so essentially this is jumping
            // in this direction: test-condition -> block
            ctx.fun.chunk.patch_jump(body_jump);
        }

        // The block start
        self.statement(ctx)?;
        ctx.emit_loop(loop_start, ctx.prev.as_ref().unwrap().line)?;

        if let Some(dist) = exit_jump {
            ctx.fun.chunk.patch_jump(dist);
            ctx.add_instruction(Instruction::Pop);
        }

        ctx.end_scope();

        Ok(())
    }

    fn expression_statement(&self, ctx: &mut Context) -> CompilationResult<()> {
        self.expression(ctx)?;
        let line = ctx.consume(Type::SemiColon, "Expect ';' after expression.")?;
        ctx.add_instruction_from(Instruction::Pop, line);
        Ok(())
    }

    fn expression(&self, ctx: &mut Context) -> CompilationResult<()> {
        // Simply parse the expression with lowest precedence
        self.parse_precedence(ctx, Precedence::Assignment)?;
        Ok(())
    }

    fn block(&self, ctx: &mut Context) -> CompilationResult<()> {
        while !ctx.check(Type::RightBrace) && !ctx.check(Type::Eof) {
            self.declaration(ctx)?;
        }

        ctx.consume(Type::RightBrace, "Expect '}' after block.")?;
        Ok(())
    }

    // To parse a function, a new context is created and do the compilation as usual. The main
    // `compile` method will branch into the correct parsing method depending on the type.
    fn function(&self, ctx: &mut Context, ftype: FunctionType) -> CompilationResult<()> {
        let stream = std::mem::replace(&mut ctx.stream, TokenStream::new(""));
        let prev = ctx.prev.take();
        let curr = ctx.curr.take();

        // Replace ctx with a new context, which means I need to push the previously processing
        // context (`current`) as the parent
        let current = std::mem::replace(ctx, Context::new(stream, ftype, None));
        ctx.prev = prev;
        ctx.curr = curr;
        ctx.has_superclass = current.has_superclass;
        ctx.is_in_class = current.is_in_class;
        ctx.enclosing = Some(Box::new(current));
        self.compile_context(ctx);

        // Now the `current` is moved to the slot `prev`
        let mut prev = ctx.enclosing.take().unwrap();
        prev.prev = ctx.prev.take();
        prev.curr = ctx.curr.take();

        // Now nested is the compiled function - moved from ctx
        let mut nested = std::mem::replace(ctx, *prev);
        ctx.stream = nested.stream;

        if !nested.errs.is_empty() {
            ctx.errs.append(&mut nested.errs);
            Err(ctx.error("Error while parsing function."))
        } else {
            let nested_as_const = ctx.fun.chunk.make_constant(ConstantValue::Fun(nested.fun));
            let const_idx = ctx.unwrap_result(nested_as_const)?;
            ctx.add_instruction(Instruction::Closure(const_idx));
            Ok(())
        }
    }

    fn method(&self, ctx: &mut Context) -> CompilationResult<()> {
        ctx.consume(Type::Identifier, "Expect method name.")?;
        let name = ctx.prev.as_ref().unwrap().lexeme.clone();
        let ftype = match name.as_str() {
            "init" => FunctionType::Initializer,
            _ => FunctionType::Method,
        };
        let name_constant = ctx.identifier_constant(name)?;

        self.function(ctx, ftype)?;
        ctx.add_instruction(Instruction::Method(name_constant));
        Ok(())
    }

    fn grouping(&self, ctx: &mut Context, _: bool) -> CompilationResult<()> {
        self.expression(ctx)?;
        ctx.consume(Type::RightParen, "Expect ')' after expression.")?;
        Ok(())
    }

    fn unary(&self, ctx: &mut Context, _: bool) -> CompilationResult<()> {
        let (ty, line) = {
            let prev = ctx.prev.as_ref().unwrap();
            (prev.ty, prev.line)
        };

        self.parse_precedence(ctx, Precedence::Unary)?;

        match ty {
            Type::Minus => {
                ctx.add_instruction_from(Instruction::Negate, line);
            }
            Type::Bang => {
                ctx.add_instruction_from(Instruction::Not, line);
            }
            _ => panic!("Unreachable"),
        }

        Ok(())
    }

    fn binary(&self, ctx: &mut Context, _: bool) -> CompilationResult<()> {
        let prev = ctx.prev.as_ref().unwrap();
        let ty = prev.ty;
        let line = prev.line;
        let rule = Self::rule(ty);

        // The second operand needs to be parsed with precedence higher than the current one.
        // Consider this expression - "2 + 3 * 10"
        // When the compiler have consumed the "+" sign, it needs to parse the whole expression
        // "3 * 10" first instead of just "3", else the operation order will be incorrect according
        // to the precedence rule.
        self.parse_precedence(ctx, rule.precedence.next())?;

        match ty {
            Type::Plus => {
                ctx.add_instruction_from(Instruction::Add, line);
            }
            Type::Minus => {
                ctx.add_instruction_from(Instruction::Subtract, line);
            }
            Type::Star => {
                ctx.add_instruction_from(Instruction::Multiply, line);
            }
            Type::Slash => {
                ctx.add_instruction_from(Instruction::Divide, line);
            }
            Type::BangEqual => {
                ctx.add_instruction_from(Instruction::Equal, line);
                ctx.add_instruction_from(Instruction::Not, line);
            }
            Type::EqualEqual => {
                ctx.add_instruction_from(Instruction::Equal, line);
            }
            Type::Greater => {
                ctx.add_instruction_from(Instruction::Greater, line);
            }
            Type::GreaterEqual => {
                ctx.add_instruction_from(Instruction::Less, line);
                ctx.add_instruction_from(Instruction::Not, line);
            }
            Type::Less => {
                ctx.add_instruction_from(Instruction::Less, line);
            }
            Type::LessEqual => {
                ctx.add_instruction_from(Instruction::Greater, line);
                ctx.add_instruction_from(Instruction::Not, line);
            }
            _ => panic!("Unreachable"),
        }

        Ok(())
    }

    fn call(&self, ctx: &mut Context, _: bool) -> CompilationResult<()> {
        let args = self.argument_list(ctx)?;
        if args.0 > 255 {
            return Err(ctx.error("Can't have more than 255 parameters."));
        }

        ctx.add_instruction(Instruction::Call(args));
        Ok(())
    }

    fn dot(&self, ctx: &mut Context, can_assign: bool) -> CompilationResult<()> {
        ctx.consume(Type::Identifier, "Expect property name after '.'.")?;
        let name = ctx.prev.as_ref().unwrap().lexeme.clone();
        let name_constant = ctx.identifier_constant(name)?;

        if can_assign && ctx.match_type(Type::Equal) {
            self.expression(ctx)?;
            ctx.add_instruction(Instruction::SetProperty(name_constant));
        } else if ctx.match_type(Type::LeftParen) {
            let args = self.argument_list(ctx)?;
            ctx.add_instruction(Instruction::Invoke(Invoke {
                name: name_constant,
                args,
            }));
        } else {
            ctx.add_instruction(Instruction::GetProperty(name_constant));
        }
        Ok(())
    }

    // When this is called, the left-hand side expression has already been compiled. If that value
    // is false, leave that value on the stack and skip the whole expression. If not, pop that
    // value from the stack and evaluate the next expression.
    fn and(&self, ctx: &mut Context, _: bool) -> CompilationResult<()> {
        let end_jump = ctx.add_instruction(Instruction::JumpIfFalse(JumpDist(0)));
        ctx.add_instruction(Instruction::Pop);
        self.parse_precedence(ctx, Precedence::And)?;
        ctx.fun.chunk.patch_jump(end_jump);
        Ok(())
    }

    // For else, if the value is false, pop the value and continue with the next expression (here
    // the next expression starts after the unconditional jump that skips to the next instruction
    // after the or expression which is taken when the value is true).
    fn or(&self, ctx: &mut Context, _: bool) -> CompilationResult<()> {
        let line = ctx.prev.as_ref().unwrap().line;
        let else_jump = ctx.add_instruction_from(Instruction::JumpIfFalse(JumpDist(0)), line);
        let end_jump = ctx.add_instruction_from(Instruction::Jump(JumpDist(0)), line);
        ctx.fun.chunk.patch_jump(else_jump);
        ctx.add_instruction_from(Instruction::Pop, line);
        self.parse_precedence(ctx, Precedence::Or)?;
        ctx.fun.chunk.patch_jump(end_jump);
        Ok(())
    }

    fn literal(&self, ctx: &mut Context, _: bool) -> CompilationResult<()> {
        let prev = ctx.prev.as_ref().unwrap();
        match prev.ty {
            Type::True => {
                ctx.add_instruction_from(Instruction::LiteralTrue, prev.line);
            }
            Type::False => {
                ctx.add_instruction_from(Instruction::LiteralFalse, prev.line);
            }
            Type::Nil => {
                ctx.add_instruction_from(Instruction::LiteralNil, prev.line);
            }
            _ => panic!("Unreachable"),
        }

        Ok(())
    }

    fn number(&self, ctx: &mut Context, _: bool) -> CompilationResult<()> {
        let prev = ctx.prev.as_ref().unwrap();
        let value = ConstantValue::from(prev.value.clone());
        let constant = ctx.fun.chunk.make_constant(value);
        let constant = ctx.unwrap_result(constant)?;
        ctx.add_instruction_from(Instruction::Constant(constant), prev.line);
        Ok(())
    }

    fn string(&self, ctx: &mut Context, _: bool) -> CompilationResult<()> {
        let prev = ctx.prev.as_ref().unwrap();
        let value = ConstantValue::from(prev.value.clone());
        let constant = ctx.fun.chunk.make_constant(value);
        let constant = ctx.unwrap_result(constant)?;
        ctx.add_instruction_from(Instruction::Constant(constant), prev.line);
        Ok(())
    }

    fn variable(&self, ctx: &mut Context, can_assign: bool) -> CompilationResult<()> {
        let prev = ctx.prev.as_ref().unwrap();
        let name = prev.lexeme.clone();
        let line = prev.line;
        self.named_variable(ctx, can_assign, name, line)
    }

    fn this(&self, ctx: &mut Context, _: bool) -> CompilationResult<()> {
        if !ctx.is_in_class {
            return Err(ctx.error("Can't use 'this' outside of a class."));
        }

        self.variable(ctx, false)
    }

    fn super_(&self, ctx: &mut Context, _: bool) -> CompilationResult<()> {
        if !ctx.is_in_class {
            return Err(ctx.error("Can't use 'super' outside of a class."));
        } else if !ctx.has_superclass {
            return Err(ctx.error("Can't use 'super' in a class with no superclass."));
        }

        ctx.consume(Type::Dot, "Expect '.' after 'super'.")?;
        ctx.consume(Type::Identifier, "Expect superclass method name.")?;

        let prev = ctx.prev.as_ref().unwrap();
        let name = prev.lexeme.clone();
        let line = prev.line;
        let name_constant = ctx.identifier_constant(name)?;

        // To access superclass' method, runtime needs access to both the receiver (`this`),
        // and the superclass of the surrounding method's class.
        self.named_variable(ctx, false, "this".to_string(), line)?;

        if ctx.match_type(Type::LeftParen) {
            let args = self.argument_list(ctx)?;
            self.named_variable(ctx, false, "super".to_string(), line)?;
            ctx.add_instruction(Instruction::SuperInvoke(Invoke {
                name: name_constant,
                args,
            }));
        } else {
            self.named_variable(ctx, false, "super".to_string(), line)?;
            ctx.add_instruction(Instruction::GetSuper(name_constant));
        }
        Ok(())
    }

    fn named_variable(
        &self,
        ctx: &mut Context,
        can_assign: bool,
        name: String,
        line: usize,
    ) -> CompilationResult<()> {
        let (set_op, get_op) = match ctx.resolve_variable(&name) {
            Ok(LocalResolution::Global) => {
                let constant = ctx.identifier_constant(name)?;
                (
                    Instruction::SetGlobal(constant),
                    Instruction::GetGlobal(constant),
                )
            }
            Ok(LocalResolution::Local(LocalIndex(index))) => (
                Instruction::SetLocal(StackPosition::Offset(index)),
                Instruction::GetLocal(StackPosition::Offset(index)),
            ),
            Ok(LocalResolution::UpValue(index)) => (
                Instruction::SetUpvalue(index),
                Instruction::GetUpvalue(index),
            ),
            Err(err) => {
                return Err(err);
            }
        };

        if can_assign && ctx.match_type(Type::Equal) {
            self.expression(ctx)?;
            ctx.add_instruction_from(set_op, line);
        } else {
            ctx.add_instruction_from(get_op, line);
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
    fn parse_precedence(&self, ctx: &mut Context, precedence: Precedence) -> CompilationResult<()> {
        ctx.advance();
        let prev = ctx.prev.as_ref().unwrap();
        let rule = Self::rule(prev.ty);

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

        match rule.prefix {
            Some(prefix) => prefix(self, ctx, can_assign)?,
            None => {
                return Err(ctx.error("Expect expression."));
            }
        }

        while precedence as usize <= Self::rule(ctx.curr.as_ref().unwrap().ty).precedence as usize {
            ctx.advance();
            let rule = Self::rule(ctx.prev.as_ref().unwrap().ty);
            (rule.infix.unwrap())(self, ctx, can_assign)?;
        }

        if can_assign && ctx.match_type(Type::Equal) {
            Err(ctx.error("Invalid assignment target."))
        } else {
            Ok(())
        }
    }

    fn parse_variable(&self, ctx: &mut Context, msg: &str) -> CompilationResult<Constant> {
        ctx.consume(Type::Identifier, msg)?;
        let prev = ctx.prev.as_ref().unwrap();
        let name = prev.lexeme.clone();

        self.declare_variable(ctx)?;
        ctx.identifier_constant(name)
    }

    fn declare_variable(&self, ctx: &mut Context) -> CompilationResult<()> {
        if ctx.scope_depth == 0 {
            return Ok(());
        }

        let name = ctx.prev.as_ref().unwrap().lexeme.clone();
        for local in ctx.locals.iter().rev() {
            // Depth = -1 means the variable has been declared but not defined. Only check the locals
            // from the current scope.
            if local.depth != -1 && local.depth < ctx.scope_depth {
                break;
            }

            if local.name == name {
                return Err(ctx.error("Already a variable with this name in this scope."));
            }
        }

        ctx.add_local(name, None)?;
        Ok(())
    }

    fn argument_list(&self, ctx: &mut Context) -> CompilationResult<ArgCount> {
        let mut count = 0;
        if !ctx.check(Type::RightParen) {
            loop {
                self.expression(ctx)?;
                count += 1;

                if !ctx.match_type(Type::Comma) {
                    break;
                }
            }
        }

        if count >= COMP_ARG_LIMIT {
            Err(ctx.error("Can't have more than 255 arguments."))
        } else {
            ctx.consume(Type::RightParen, "Expect ')' after arguments.")?;
            Ok(ArgCount(count))
        }
    }

    // Synchronize the token stream if an error is found during compilation, and the course of action
    // is to simply skip all tokens until the end of statement or the eof is found.
    fn synchronize(&self, ctx: &mut Context) {
        while ctx.curr.as_ref().unwrap().ty != Type::Eof {
            if let Type::SemiColon = ctx.prev.as_ref().unwrap().ty {
                return;
            }

            match ctx.curr.as_ref().unwrap().ty {
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
                _ => ctx.advance(),
            }
        }
    }
}
