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

#[derive(Debug, PartialEq)]
pub enum Literal {
    Str(String),
    Num(f64),
    Bool(bool),
    Nil,
}

#[derive(Debug, PartialEq)]
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
