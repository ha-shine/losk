use crate::chunk::{Chunk, Instruction};
use crate::error::Error;
use crate::instruction::Constant;
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

struct Context<'a> {
    stream: TokenStream<'a>,
    chunk: Chunk,

    curr: Option<Token>,
    prev: Option<Token>,

    errs: Vec<Error>,
    panic: bool,
}

type ParseFn<'a> = fn(&mut Context<'a>, bool);

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
    // A Pratt parser's table, where each row map to token type as index. The first column
    // maps a token type to prefix parsing function while the second column maps to infix parsing
    // function. The third column precedence represents the precedence of the infix expression
    // which uses that token as an operator.
    // Precedence of the infix operators don't need to be tracked since the precedence of all prefix
    // operators are the same.
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
        ParseRule(None, None, Precedence::None),                 // And
        ParseRule(None, None, Precedence::None),                 // Class
        ParseRule(None, None, Precedence::None),                 // Else
        ParseRule(Some(Self::literal), None, Precedence::None),  // True
        ParseRule(Some(Self::literal), None, Precedence::None),  // False
        ParseRule(None, None, Precedence::None),                 // For
        ParseRule(None, None, Precedence::None),                 // Fun
        ParseRule(None, None, Precedence::None),                 // If
        ParseRule(Some(Self::literal), None, Precedence::None),  // Nil
        ParseRule(None, None, Precedence::None),                 // Or
        ParseRule(None, None, Precedence::None),                 // Print
        ParseRule(None, None, Precedence::None),                 // Return
        ParseRule(None, None, Precedence::None),                 // Super
        ParseRule(None, None, Precedence::None),                 // This
        ParseRule(None, None, Precedence::None),                 // Var
        ParseRule(None, None, Precedence::None),                 // While
        ParseRule(None, None, Precedence::None),                 // Error
    ];

    fn compiled(mut stream: TokenStream<'a>) -> Self {
        let mut ctx = Context {
            stream,
            chunk: Chunk::new(),

            curr: None,
            prev: None,

            errs: Vec::new(),
            panic: false,
        };

        ctx.compile();
        ctx
    }

    fn compile(&mut self) {
        self.advance();

        // Maybe check the result at this level and do the synchronisation here?
        while !self.match_type(Type::Eof) {
            self.declaration();
        }
    }

    fn declaration(&mut self) {
        if self.match_type(Type::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic {
            self.synchronize();
        }
    }

    fn var_declaration(&mut self) {
        let constant = self.parse_variable("Expect variable name.");

        if self.match_type(Type::Equal) {
            self.expression();
        } else {
            self.chunk
                .add_instruction(Instruction::LiteralNil, self.prev.as_ref().unwrap().line);
        }

        let line = self.consume(Type::SemiColon, "Expect ';' after variable declaration.");
        self.define_variable(constant, line);
    }

    fn statement(&mut self) {
        if self.match_type(Type::Print) {
            self.print_statement();
        } else {
            self.expression_statement();
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        let line = self.consume(Type::SemiColon, "Expect ';' after value.");
        self.chunk.add_instruction(Instruction::Print, line);
    }

    fn expression_statement(&mut self) {
        self.expression();
        let line = self.consume(Type::SemiColon, "Expect ';' after expression.");
        self.chunk.add_instruction(Instruction::Pop, line);
    }

    fn expression(&mut self) {
        // Simply parse the expression with lowest precedence
        self.parse_precedence(Precedence::Assignment);
    }

    fn grouping(&mut self, _: bool) {
        self.expression();
        self.consume(Type::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self, _: bool) {
        let (ty, line) = {
            let prev = self.prev.as_ref().unwrap();
            (prev.ty, prev.line)
        };

        self.parse_precedence(Precedence::Unary);

        match ty {
            Type::Minus => self.chunk.add_instruction(Instruction::Negate, line),
            Type::Bang => self.chunk.add_instruction(Instruction::Not, line),
            _ => panic!("Unreachable"),
        }
    }

    fn binary(&mut self, _: bool) {
        let prev = self.prev.as_ref().unwrap();
        let ty = prev.ty;
        let line = prev.line;
        let rule = &Self::PARSE_RULES[ty as usize];

        // The second operand needs to be parsed with precedence higher than the current one.
        // Consider this expression - "2 + 3 * 10"
        // When the compiler have consumed the "+" sign, it needs to parse the whole expression
        // "3 * 10" first instead of just "3", else the operation order will be incorrect according
        // to the precedence rule.
        self.parse_precedence(rule.precedence().next());

        match ty {
            Type::Plus => self.chunk.add_instruction(Instruction::Add, line),
            Type::Minus => self.chunk.add_instruction(Instruction::Subtract, line),
            Type::Star => self.chunk.add_instruction(Instruction::Multiply, line),
            Type::Slash => self.chunk.add_instruction(Instruction::Divide, line),
            Type::BangEqual => {
                self.chunk.add_instruction(Instruction::Equal, line);
                self.chunk.add_instruction(Instruction::Not, line);
            }
            Type::EqualEqual => self.chunk.add_instruction(Instruction::Equal, line),
            Type::Greater => self.chunk.add_instruction(Instruction::Greater, line),
            Type::GreaterEqual => {
                self.chunk.add_instruction(Instruction::Less, line);
                self.chunk.add_instruction(Instruction::Not, line);
            }
            Type::Less => self.chunk.add_instruction(Instruction::Less, line),
            Type::LessEqual => {
                self.chunk.add_instruction(Instruction::Greater, line);
                self.chunk.add_instruction(Instruction::Not, line);
            }
            _ => panic!("Unreachable"),
        }
    }

    fn literal(&mut self, _: bool) {
        let prev = self.prev.as_ref().unwrap();
        match prev.ty {
            Type::True => self
                .chunk
                .add_instruction(Instruction::LiteralTrue, prev.line),
            Type::False => self
                .chunk
                .add_instruction(Instruction::LiteralFalse, prev.line),
            Type::Nil => self
                .chunk
                .add_instruction(Instruction::LiteralNil, prev.line),
            _ => panic!("Unreachable"),
        }
    }

    fn number(&mut self, _: bool) {
        let prev = self.prev.as_ref().unwrap();
        let value = Value::from(prev.value.clone());
        let constant = self.chunk.make_constant(value).unwrap();
        self.chunk
            .add_instruction(Instruction::Constant(constant), prev.line);
    }

    fn string(&mut self, _: bool) {
        let prev = self.prev.as_ref().unwrap();
        let value = Value::from(prev.value.clone());
        let constant = self.chunk.make_constant(value).unwrap();
        self.chunk
            .add_instruction(Instruction::Constant(constant), prev.line);
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(can_assign);
    }

    // TODO: Both get and define global will create a new constant item even if the get is only
    //   reusing the previously declared name. The chunk should be refactored to have a global
    //   map as well, and returned the previous index if it's already defined.
    fn named_variable(&mut self, can_assign: bool) {
        let prev = self.prev.as_ref().unwrap();
        let name = prev.lexeme.clone();
        let line = prev.line;
        let constant = self.identifier_constant(name);

        if can_assign && self.match_type(Type::Equal) {
            self.expression();
            self.chunk
                .add_instruction(Instruction::SetGlobal(constant), line);
        } else {
            self.chunk
                .add_instruction(Instruction::GetGlobal(constant), line);
        }
    }

    // This will parse an expression with precedence higher than the given one starting at the
    // current token.
    // The first token will always belong to some kind of prefix expression, otherwise it's a syntax
    // error. (Think about how this method is called in `expression()`).
    // After parsing with prefix function, check if next token's infix operation precedence is
    // higher than the current precedence defined. As long as the next token's infix precedence
    // is higher, the parser will keep parsing those expression until none is left.
    fn parse_precedence(&mut self, precedence: Precedence) {
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
            Some(prefix) => prefix(self, can_assign),
            None => {
                self.error_at("Expect expression.");
            }
        }

        while precedence as usize
            <= *Self::PARSE_RULES[self.curr.as_ref().unwrap().ty as usize].precedence() as usize
        {
            self.advance();
            let rule = &Self::PARSE_RULES[self.prev.as_ref().unwrap().ty as usize];
            (rule.infix().unwrap())(self, false);
        }

        if can_assign && self.match_type(Type::Equal) {
            self.error_at("Invalid assignment target.");
        }
    }

    fn parse_variable(&mut self, msg: &str) -> Constant {
        self.consume(Type::Identifier, msg);
        let prev = self.prev.as_ref().unwrap();
        self.identifier_constant(prev.lexeme.clone())
    }

    fn identifier_constant(&mut self, name: String) -> Constant {
        self.chunk.make_constant(Value::Str(name)).unwrap()
    }

    fn define_variable(&mut self, constant: Constant, line: usize) {
        self.chunk
            .add_instruction(Instruction::DefineGlobal(constant), line);
    }

    fn advance(&mut self) {
        self.prev = self.curr.take();
        self.curr = self.stream.next();
    }

    // Consume will return the line number of the consumed token for ease.
    // Maybe I should return the reference to the token as a result instead of just the line number.
    // This way, I can bubble up the result back to the main compile function too.
    fn consume(&mut self, ty: Type, msg: &str) -> usize {
        match &self.curr {
            Some(token) => {
                let line = token.line;

                if token.ty == ty {
                    self.advance();
                } else {
                    self.error_at(msg);
                }

                line
            }
            None => {
                self.error_at(msg);
                0
            }
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

    fn check(&self, ty: Type) -> bool {
        self.curr.as_ref().unwrap().ty == ty
    }

    fn error_at(&mut self, msg: &str) {
        if self.panic {
            return;
        }

        self.panic = true;

        let line = if let Some(curr) = &self.curr {
            curr.line
        } else {
            0
        };

        self.errs.push(Error::CompileError {
            line,
            msg: String::from(msg),
        })
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

        dbg!(res);
    }
}
