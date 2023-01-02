use crate::callable::Callable;
use crate::instance::Instance;
use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ptr;
use std::rc::Rc;

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum Type {
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

#[derive(Debug, Clone)]
pub(crate) enum Literal {
    Callable(Rc<dyn Callable>),
    Instance(Rc<RefCell<Instance>>),
    Str(Rc<String>),
    Num(f64),
    Bool(bool),
    Nil,
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Literal::Callable(lhs), Literal::Callable(rhs)) => ptr::eq(lhs, rhs),
            (Literal::Str(lhs), Literal::Str(rhs)) => lhs == rhs,
            (Literal::Num(lhs), Literal::Num(rhs)) => lhs == rhs,
            (Literal::Bool(lhs), Literal::Bool(rhs)) => lhs == rhs,
            (Literal::Nil, Literal::Nil) => true,
            (Literal::Nil, _) => false,
            (_, Literal::Nil) => false,
            _ => false,
        }
    }
}

impl Eq for Literal {}

impl Hash for Literal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Literal::Callable(ptr) => Rc::as_ptr(ptr).hash(state),
            Literal::Instance(ptr) => Rc::as_ptr(ptr).hash(state),
            Literal::Str(val) => val.hash(state),
            Literal::Num(val) => val.to_bits().hash(state),
            Literal::Bool(val) => val.hash(state),
            Literal::Nil => 0.hash(state),
        }
    }
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
            Literal::Callable(val) => {
                write!(f, "{:?}", val)
            }
            Literal::Instance(instance) => {
                write!(f, "{}", RefCell::borrow(instance))
            }
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub(crate) ty: Type,
    pub(crate) lexeme: String,
    pub(crate) line: usize,
    pub(crate) col: usize,
    pub(crate) value: Literal,
}

impl Token {
    pub(crate) fn new(ty: Type, lexeme: String, line: usize, col: usize, value: Literal) -> Self {
        Token {
            ty,
            lexeme,
            line,
            col,
            value,
        }
    }
}
