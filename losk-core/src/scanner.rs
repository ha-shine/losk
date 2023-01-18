use phf::{phf_map, Map};

use crate::error::Error;
use crate::token::{Literal, Token, Type};

pub struct Scanner;

impl Scanner {
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

    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Scanner
    }

    pub fn scan_tokens<'a, 'b>(&'a mut self, src: &'b str) -> TokenStream
    where
        'b: 'a,
    {
        let stream = TokenStream::new(src);
        stream
    }
}

pub struct TokenStream<'a> {
    src: &'a str,
    line: usize,

    // `start` and `current` points to the start and end of the token being scanned
    start: usize,
    current: usize,

    // This represents a token's index in the token stream
    index: usize,

    // This flag is set to `true` if the eof is reached and the eof token has been emitted.
    // This is required because the iterator needs to distinguish between when eof is reached but
    // the token is not emitted, and eof is reached and token has been emitted.
    eof: bool,
    error: Option<Error>,
}

impl<'a> TokenStream<'a> {
    pub fn new(src: &'a str) -> Self {
        TokenStream {
            src,
            line: 0,
            start: 0,
            current: 0,
            index: 0,
            eof: false,
            error: None,
        }
    }

    pub fn error(&self) -> Option<&Error> {
        self.error.as_ref()
    }

    fn scan_token(&mut self) -> Result<Option<Token>, Error> {
        let c = self.advance();

        let token = match c {
            '(' => Some(self.make_token(Type::LeftParen)),
            ')' => Some(self.make_token(Type::RightParen)),
            '{' => Some(self.make_token(Type::LeftBrace)),
            '}' => Some(self.make_token(Type::RightBrace)),
            ',' => Some(self.make_token(Type::Comma)),
            '.' => Some(self.make_token(Type::Dot)),
            '-' => Some(self.make_token(Type::Minus)),
            '+' => Some(self.make_token(Type::Plus)),
            ';' => Some(self.make_token(Type::SemiColon)),
            '*' => Some(self.make_token(Type::Star)),

            '!' => {
                if self.match_char('=') {
                    Some(self.make_token(Type::BangEqual))
                } else {
                    Some(self.make_token(Type::Bang))
                }
            }

            '=' => {
                if self.match_char('=') {
                    Some(self.make_token(Type::EqualEqual))
                } else {
                    Some(self.make_token(Type::Equal))
                }
            }

            '<' => {
                if self.match_char('=') {
                    Some(self.make_token(Type::LessEqual))
                } else {
                    Some(self.make_token(Type::Less))
                }
            }

            '>' => {
                if self.match_char('=') {
                    Some(self.make_token(Type::GreaterEqual))
                } else {
                    Some(self.make_token(Type::Greater))
                }
            }

            '/' => {
                if self.match_char('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                    None
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
                        None
                    } else {
                        return Err(Error::UnterminatedBlockComment { line: self.line });
                    }
                } else {
                    Some(self.make_token(Type::Slash))
                }
            }

            '"' => Some(self.string()?),

            // White spaces, do nothing
            ' ' | '\t' | '\r' => None,

            // Increment for new line
            '\n' => {
                self.line += 1;
                None
            }

            _ => {
                if c.is_ascii_digit() {
                    Some(self.number()?)
                } else if c.is_alphabetic() {
                    Some(self.identifier()?)
                } else {
                    return Err(Error::UnexpectedCharacter {
                        ch: c,
                        line: self.line,
                    });
                }
            }
        };

        Ok(token)
    }

    fn string(&mut self) -> Result<Token, Error> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }

            self.advance();
        }

        if self.is_at_end() {
            return Err(Error::UnterminatedString { line: self.line });
        }

        // consume the closing "
        self.advance();
        Ok(self.make_token_with_val(
            Type::String,
            Literal::from(String::from(&self.src[self.start + 1..self.current - 1])),
        ))
    }

    fn number(&mut self) -> Result<Token, Error> {
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

    fn identifier(&mut self) -> Result<Token, Error> {
        while self.peek().is_alphanumeric() {
            self.advance();
        }

        let text = String::from(&self.src[self.start..self.current]);

        match Scanner::KEYWORDS.get(&text) {
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

    fn is_at_end(&self) -> bool {
        self.current >= self.src.len()
    }

    fn make_token(&mut self, ty: Type) -> Token {
        self.make_token_with_val(ty, Literal::Nil)
    }

    fn make_token_with_val(&mut self, ty: Type, val: Literal) -> Token {
        let lexeme = match ty {
            Type::Eof => String::new(),
            _ => String::from(&self.src[self.start..self.current]),
        };

        let token = Token::new(ty, lexeme, self.line, self.start, self.index, val);
        self.index += 1;
        token
    }
}

impl<'a> Iterator for TokenStream<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.eof || self.error.is_some() {
            return None;
        }

        while !self.is_at_end() {
            self.start = self.current;

            let token = self.scan_token();
            match token {
                Ok(None) => continue,
                Ok(Some(token)) => return Some(token),
                Err(err) => {
                    self.error = Some(err);
                    return None;
                }
            }
        }

        self.eof = true;
        Some(self.make_token(Type::Eof))
    }
}

#[cfg(test)]
mod tests {
    use crate::error::Error;
    use crate::scanner::Scanner;
    use crate::token::{Literal, Token, Type};

    #[test]
    fn test_basic_scanning() {
        let source = "class fun {} var foo bar 12.45 \"hello\" true false nil // this is a comment";
        let mut scanner = Scanner::new();
        let stream = scanner.scan_tokens(source);

        assert_eq!(
            stream.collect::<Vec<Token>>(),
            vec![
                Token::new(Type::Class, String::from("class"), 0, 0, 0, Literal::Nil),
                Token::new(Type::Fun, String::from("fun"), 0, 6, 1, Literal::Nil),
                Token::new(Type::LeftBrace, String::from("{"), 0, 10, 2, Literal::Nil),
                Token::new(Type::RightBrace, String::from("}"), 0, 11, 3, Literal::Nil),
                Token::new(Type::Var, String::from("var"), 0, 13, 4, Literal::Nil),
                Token::new(
                    Type::Identifier,
                    String::from("foo"),
                    0,
                    17,
                    5,
                    Literal::Nil
                ),
                Token::new(
                    Type::Identifier,
                    String::from("bar"),
                    0,
                    21,
                    6,
                    Literal::Nil
                ),
                Token::new(
                    Type::Number,
                    String::from("12.45"),
                    0,
                    25,
                    7,
                    Literal::Num(12.45)
                ),
                Token::new(
                    Type::String,
                    String::from("\"hello\""),
                    0,
                    31,
                    8,
                    Literal::from("hello")
                ),
                Token::new(
                    Type::True,
                    String::from("true"),
                    0,
                    39,
                    9,
                    Literal::Bool(true)
                ),
                Token::new(
                    Type::False,
                    String::from("false"),
                    0,
                    44,
                    10,
                    Literal::Bool(false)
                ),
                Token::new(Type::Nil, String::from("nil"), 0, 50, 11, Literal::Nil),
                Token::new(Type::Eof, String::new(), 0, 54, 12, Literal::Nil),
            ]
        );
    }

    #[test]
    fn test_multiline_comment() {
        let source = "/*\n\
            this is a multiline comment \n\
        */";
        let mut scanner = Scanner::new();
        let stream = scanner.scan_tokens(source);

        assert_eq!(
            stream.collect::<Vec<Token>>(),
            vec![Token::new(Type::Eof, String::new(), 2, 0, 0, Literal::Nil)]
        );
    }

    #[test]
    fn test_unterminated_multiline_comment() {
        let source = "/*";
        let mut scanner = Scanner::new();
        let mut stream = scanner.scan_tokens(source);
        stream.by_ref().last();

        assert_eq!(
            stream.error().unwrap(),
            &Error::UnterminatedBlockComment { line: 0 }
        );
    }

    #[test]
    fn test_unterminated_string() {
        let source = "\"hello";
        let mut scanner = Scanner::new();
        let mut stream = scanner.scan_tokens(source);
        stream.by_ref().last();

        assert_eq!(
            stream.error().unwrap(),
            &Error::UnterminatedString { line: 0 }
        );
    }
}
