use phf::{phf_map, Map};

use crate::errors::LoskError;
use crate::token::{Literal, Token, Type};

pub struct Scanner<'a> {
    start: usize,
    current: usize,
    line: usize,
    src: &'a str,
}

impl<'a> Scanner<'a> {
    const KEYWORDS: Map<&'static str, Type> = phf_map! {
        "and" => Type::And,
        "else" => Type::Else,
        "for" => Type::For,
        "if" => Type::If,
        "or" => Type::Or,
        "return" => Type::Return,
        "this" => Type::This,
        "var" => Type::Var,
        "class" => Type::Class,
        "false" => Type::False,
        "fun" => Type::Fun,
        "nil" => Type::Nil,
        "print" => Type::Print,
        "super" => Type::Super,
        "true" => Type::True,
        "while" => Type::While,
    };

    pub fn new(src: &'a str) -> Self {
        Scanner {
            start: 0,
            current: 0,
            line: 0,
            src,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, LoskError> {
        let mut tokens = vec![];
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token(&mut tokens)?;
        }

        tokens.push(self.make_token(Type::Eof));
        Ok(tokens)
    }

    fn scan_token(&mut self, tokens: &mut Vec<Token>) -> Result<(), LoskError> {
        let c = self.advance();

        match c {
            '(' => tokens.push(self.make_token(Type::LeftParen)),
            ')' => tokens.push(self.make_token(Type::RightParen)),
            '{' => tokens.push(self.make_token(Type::LeftBrace)),
            '}' => tokens.push(self.make_token(Type::RightBrace)),
            ',' => tokens.push(self.make_token(Type::Comma)),
            '.' => tokens.push(self.make_token(Type::Dot)),
            '-' => tokens.push(self.make_token(Type::Minus)),
            '+' => tokens.push(self.make_token(Type::Plus)),
            ';' => tokens.push(self.make_token(Type::SemiColon)),
            '*' => tokens.push(self.make_token(Type::Star)),

            '!' => {
                if self.match_char('=') {
                    tokens.push(self.make_token(Type::BangEqual))
                } else {
                    tokens.push(self.make_token(Type::Bang))
                }
            }

            '=' => {
                if self.match_char('=') {
                    tokens.push(self.make_token(Type::EqualEqual))
                } else {
                    tokens.push(self.make_token(Type::Equal))
                }
            }

            '<' => {
                if self.match_char('=') {
                    tokens.push(self.make_token(Type::LessEqual))
                } else {
                    tokens.push(self.make_token(Type::Less))
                }
            }

            '>' => {
                if self.match_char('=') {
                    tokens.push(self.make_token(Type::GreaterEqual))
                } else {
                    tokens.push(self.make_token(Type::Greater))
                }
            }

            '/' => {
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else if self.match_char('*') {
                    let mut done = false;
                    while !self.is_at_end() && !done {
                        // Multi-line comments are allowed, so ensure to increment the line since
                        // `advance()` doesn't increment line number
                        if self.current() == '\n' {
                            self.line += 1;
                        }

                        let now = self.advance();
                        if now == '*' && self.peek() == '/' {
                            self.advance();
                            done = true;
                        }
                    }

                    if done {
                        return Ok(());
                    }

                    return Err(self.error("Unterminated block comment."));
                } else {
                    tokens.push(self.make_token(Type::Slash));
                }
            }

            '"' => tokens.push(self.string()?),

            // White spaces, do nothing
            ' ' | '\t' | '\r' => {}

            // Increment for new line
            '\n' => self.line += 1,

            _ => {
                if c.is_ascii_digit() {
                    tokens.push(self.number()?)
                } else if c.is_alphabetic() {
                    tokens.push(self.identifier()?)
                } else {
                    return Err(self.error("Unexpected character."));
                }
            }
        }

        Ok(())
    }

    fn string(&mut self) -> Result<Token, LoskError> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            return Err(self.error("Unterminated string."));
        }

        // consume the closing "
        self.advance();
        Ok(self.make_token_with_val(
            Type::String,
            Literal::from(String::from(&self.src[self.start + 1..self.current - 1])),
        ))
    }

    fn number(&mut self) -> Result<Token, LoskError> {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            self.advance();

            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        Ok(self.make_token_with_val(
            Type::Number,
            Literal::Num(self.src[self.start..self.current].parse::<f64>().unwrap()),
        ))
    }

    fn identifier(&mut self) -> Result<Token, LoskError> {
        while self.peek().is_alphanumeric() {
            self.advance();
        }

        let text = String::from(&self.src[self.start..self.current]);

        match Self::KEYWORDS.get(&text) {
            None => Ok(self.make_token(Type::Identifier)),
            Some(ty @ Type::True) | Some(ty @ Type::False) => {
                let val = match ty {
                    Type::True => Literal::Bool(true),
                    _ => Literal::Bool(false),
                };

                Ok(self.make_token_with_val(*ty, val))
            }
            Some(keyword) => Ok(self.make_token(*keyword)),
        }
    }

    fn make_token(&self, ty: Type) -> Token {
        self.make_token_with_val(ty, Literal::Nil)
    }

    fn make_token_with_val(&self, ty: Type, val: Literal) -> Token {
        let lexeme = match ty {
            Type::Eof => String::new(),
            _ => String::from(&self.src[self.start..self.current]),
        };

        Token::new(ty, lexeme, self.line, val)
    }

    fn current(&self) -> char {
        self.src.chars().nth(self.current).unwrap()
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.current()
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.src.len() {
            '\0'
        } else {
            self.src.chars().nth(self.current + 1).unwrap()
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.src.len()
    }

    fn advance(&mut self) -> char {
        let res = self.current();
        self.current += 1;
        res
    }

    fn match_char(&mut self, c: char) -> bool {
        if self.is_at_end() || self.current() != c {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn error(&self, msg: &str) -> LoskError {
        LoskError::ScannerError {
            line: self.line,
            msg: String::from(msg),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::scanner::LoskError::ScannerError;
    use crate::scanner::Scanner;
    use crate::token::{Literal, Token, Type};

    #[test]
    fn test_basic_scanning() {
        let source = "class fun {} var foo bar 12.45 \"hello\" true false nil // this is a comment";
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();

        assert_eq!(
            tokens,
            vec![
                Token::new(Type::Class, String::from("class"), 0, Literal::Nil),
                Token::new(Type::Fun, String::from("fun"), 0, Literal::Nil),
                Token::new(Type::LeftBrace, String::from("{"), 0, Literal::Nil),
                Token::new(Type::RightBrace, String::from("}"), 0, Literal::Nil),
                Token::new(Type::Var, String::from("var"), 0, Literal::Nil),
                Token::new(Type::Identifier, String::from("foo"), 0, Literal::Nil),
                Token::new(Type::Identifier, String::from("bar"), 0, Literal::Nil),
                Token::new(Type::Number, String::from("12.45"), 0, Literal::Num(12.45)),
                Token::new(
                    Type::String,
                    String::from("\"hello\""),
                    0,
                    Literal::from("hello")
                ),
                Token::new(Type::True, String::from("true"), 0, Literal::Bool(true)),
                Token::new(Type::False, String::from("false"), 0, Literal::Bool(false)),
                Token::new(Type::Nil, String::from("nil"), 0, Literal::Nil),
                Token::new(Type::Eof, String::new(), 0, Literal::Nil),
            ]
        );
    }

    #[test]
    fn test_multiline_comment() {
        let source = "/*\n\
            this is a multiline comment \n\
        */";
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();

        assert_eq!(
            tokens,
            vec![Token::new(Type::Eof, String::new(), 2, Literal::Nil)]
        );
    }

    #[test]
    fn test_unterminated_multiline_comment() {
        let source = "/*";
        let mut scanner = Scanner::new(source);
        assert_eq!(
            scanner.scan_tokens(),
            Err(ScannerError {
                line: 0,
                msg: String::from("Unterminated block comment.")
            })
        );
    }

    #[test]
    fn test_unterminated_string() {
        let source = "\"hello";
        let mut scanner = Scanner::new(source);
        assert_eq!(
            scanner.scan_tokens(),
            Err(ScannerError {
                line: 0,
                msg: String::from("Unterminated string.")
            })
        );
    }
}
