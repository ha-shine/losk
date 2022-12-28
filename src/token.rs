use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Type {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    SemiColon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier,
    String,
    Number,

    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Str(Rc<String>),
    Num(f64),
    Bool(bool),
    Nil,
}

impl From<bool> for Literal {
    fn from(value: bool) -> Self {
        Literal::Bool(value)
    }
}

impl From<String> for Literal {
    fn from(value: String) -> Self {
        Literal::Str(Rc::new(value))
    }
}

impl From<&str> for Literal {
    fn from(value: &str) -> Self {
        Literal::Str(Rc::new(String::from(value)))
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Str(val) => write!(f, "{}", val),
            Literal::Num(val) => write!(f, "{}", val),
            Literal::Bool(val) => write!(f, "{}", val),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

macro_rules! impl_from_num_for_literal {
    ( $( $t:ident )* ) => {
        $(
            impl From<$t> for Literal {
                fn from(n: $t) -> Literal {
                    Literal::Num(n as f64)
                }
            }
        )*
    }
}

impl_from_num_for_literal!(u8 i8 u16 i16 u32 i32 u64 i64 u128 i128 usize isize f32 f64);

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub(crate) ty: Type,
    pub(crate) lexeme: String,
    pub(crate) line: usize,
    pub(crate) value: Literal,
}

impl Token {
    pub(crate) fn new(ty: Type, lexeme: String, line: usize, value: Literal) -> Self {
        Token {
            ty,
            lexeme,
            line,
            value,
        }
    }
}
