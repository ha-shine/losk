use crate::chunk::{Chunk, Instruction};
use crate::error::Error;
use crate::value::Value;
use core::{Token, TokenStream, Type};

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

impl Compiler {
    fn new() -> Self {
        Compiler
    }

    fn compile(&self, stream: TokenStream) -> Result<Chunk, Vec<Error>> {
        let mut ctx = Context::compiled(stream);

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

type ParseFn<'a> = fn(&mut Context<'a>);

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
        ParseRule(None, None, Precedence::None),                 // Identifier
        ParseRule(None, None, Precedence::None),                 // String
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
        let curr = stream.next();
        let mut ctx = Context {
            stream,
            chunk: Chunk::new(),

            curr,
            prev: None,

            errs: Vec::new(),
            panic: false,
        };

        ctx.compile();
        ctx
    }

    // NOTE: In Textbook, makeConstant() create a constant and returns the index while emitConstant()
    //  will call this and add the instruction.
    fn compile(&mut self) {
        self.expression();
    }

    fn expression(&mut self) {
        // Simply parse the expression with lowest precedence
        self.parse_precedence(Precedence::Assignment);
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(Type::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self) {
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

    fn binary(&mut self) {
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

    fn literal(&mut self) {
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

    fn number(&mut self) {
        let prev = self.prev.as_ref().unwrap();
        let value = Value::from(prev.value.clone());
        self.chunk.add_constant(value, prev.line).unwrap();
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

        match rule.prefix() {
            Some(prefix) => prefix(self),
            None => {
                self.error_at("Expect expression.");
            }
        }

        while precedence as usize
            <= *Self::PARSE_RULES[self.curr.as_ref().unwrap().ty as usize].precedence() as usize
        {
            self.advance();
            let rule = &Self::PARSE_RULES[self.prev.as_ref().unwrap().ty as usize];
            (rule.infix().unwrap())(self);
        }
    }

    fn advance(&mut self) {
        self.prev = self.curr.take();
        self.curr = self.stream.next();
    }

    fn consume(&mut self, ty: Type, msg: &str) {
        match &self.curr {
            Some(token) => {
                if token.ty == ty {
                    self.advance();
                } else {
                    self.error_at(msg)
                }
            }
            None => self.error_at(msg),
        }
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
}

#[cfg(test)]
mod tests {
    use crate::compiler::Compiler;
    use core::*;

    #[test]
    fn test_compilation() {
        let mut scanner = Scanner::new();
        let stream = scanner.scan_tokens("(-1 + 2) * 3 - -4");
        let compiler = Compiler::new();
        let res = compiler.compile(stream);

        dbg!(res);
    }
}
