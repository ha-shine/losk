use core::Literal;
use std::fmt::{Display, Formatter};
use std::ops::{Add, Div, Mul, Neg, Not, Sub};

#[derive(Copy, Clone, Debug)]
pub(crate) enum Value {
    Double(f64),
    Bool(bool),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Double(val) => write!(f, "{}", val),
            Value::Bool(val) => write!(f, "{}", val),
            Value::Nil => write!(f, "nil"),
        }
    }
}

impl Add for Value {
    type Output = Result<Value, &'static str>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Double(lhs), Value::Double(rhs)) => Ok(Value::Double(lhs + rhs)),
            (_, _) => Err("Expect string or number operands for both."),
        }
    }
}

impl Sub for Value {
    type Output = Result<Value, &'static str>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Double(lhs), Value::Double(rhs)) => Ok(Value::Double(lhs - rhs)),
            (_, _) => Err("Expect string or number operands for both."),
        }
    }
}

impl Mul for Value {
    type Output = Result<Value, &'static str>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Double(lhs), Value::Double(rhs)) => Ok(Value::Double(lhs * rhs)),
            (_, _) => Err("Expect string or number operands for both."),
        }
    }
}

impl Div for Value {
    type Output = Result<Value, &'static str>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Double(lhs), Value::Double(rhs)) => Ok(Value::Double(lhs / rhs)),
            (_, _) => Err("Expect string or number operands for both."),
        }
    }
}

impl Neg for Value {
    type Output = Result<Value, &'static str>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Double(val) => Ok(Value::Double(-val)),
            _ => Err("Expect number operand."),
        }
    }
}

impl Not for Value {
    type Output = Result<Value, &'static str>;

    fn not(self) -> Self::Output {
        match self {
            Value::Bool(val) => Ok(Value::Bool(!val)),
            _ => Err("Expect boolean operand."),
        }
    }
}

impl From<Literal> for Value {
    fn from(value: Literal) -> Self {
        match value {
            Literal::Str(_) => Value::Nil,
            Literal::Num(val) => Value::Double(val),
            Literal::Bool(_) => Value::Nil,
            Literal::Nil => Value::Nil,
        }
    }
}
