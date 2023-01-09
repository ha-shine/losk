use core::Literal;
use std::fmt::{Display, Formatter};
use std::ops::{Add, Div, Mul, Neg, Not, Sub};

// These values here are emitted from the source code into the chunk by the compiler.
// The chunk store them in a constant pool which are referred by their index when the vm executes
// the bytecode (which is the chunk).
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Value<'a> {
    Double(f64),
    Bool(bool),
    Str(&'a str),
    Nil,
}

impl<'a> Value<'a> {
    pub(crate) fn greater(self, other: Self) -> Result<Value<'a>, &'static str> {
        match (self, other) {
            (Value::Double(lhs), Value::Double(rhs)) => Ok(Value::Bool(lhs > rhs)),
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(Value::Bool(lhs & !rhs)),
            (Value::Nil, Value::Nil) => Ok(Value::Bool(false)),
            (_, _) => Err("Expect the operands to be of same type."),
        }
    }

    pub(crate) fn less(self, other: Self) -> Result<Value<'a>, &'static str> {
        match (self, other) {
            (Value::Double(lhs), Value::Double(rhs)) => Ok(Value::Bool(lhs < rhs)),
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(Value::Bool(!lhs & rhs)),
            (Value::Nil, Value::Nil) => Ok(Value::Bool(false)),
            (_, _) => Err("Expect the operands to be of same type."),
        }
    }
}

impl<'a> Display for Value<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Double(val) => write!(f, "{}", val),
            Value::Bool(val) => write!(f, "{}", val),
            Value::Str(val) => write!(f, "{}", val),
            Value::Nil => write!(f, "nil"),
        }
    }
}

impl<'a> Add for Value<'a> {
    type Output = Result<Value<'a>, &'static str>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Double(lhs), Value::Double(rhs)) => Ok(Value::Double(lhs + rhs)),
            (_, _) => Err("Expect string or number operands for both."),
        }
    }
}

impl<'a> Sub for Value<'a> {
    type Output = Result<Value<'a>, &'static str>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Double(lhs), Value::Double(rhs)) => Ok(Value::Double(lhs - rhs)),
            (_, _) => Err("Expect string or number operands for both."),
        }
    }
}

impl<'a> Mul for Value<'a> {
    type Output = Result<Value<'a>, &'static str>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Double(lhs), Value::Double(rhs)) => Ok(Value::Double(lhs * rhs)),
            (_, _) => Err("Expect string or number operands for both."),
        }
    }
}

impl<'a> Div for Value<'a> {
    type Output = Result<Value<'a>, &'static str>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Double(lhs), Value::Double(rhs)) => Ok(Value::Double(lhs / rhs)),
            (_, _) => Err("Expect string or number operands for both."),
        }
    }
}

impl<'a> Neg for Value<'a> {
    type Output = Result<Value<'a>, &'static str>;

    fn neg(self) -> Self::Output {
        match self {
            Value::Double(val) => Ok(Value::Double(-val)),
            _ => Err("Expect number operand."),
        }
    }
}

impl<'a> Not for Value<'a> {
    type Output = Result<Value<'a>, &'static str>;

    fn not(self) -> Self::Output {
        match self {
            Value::Bool(val) => Ok(Value::Bool(!val)),
            _ => Err("Expect boolean operand."),
        }
    }
}

impl<'a> From<Literal> for Value<'a> {
    fn from(value: Literal) -> Self {
        match value {
            Literal::Str(_) => Value::Nil,
            Literal::Num(val) => Value::Double(val),
            Literal::Bool(_) => Value::Nil,
            Literal::Nil => Value::Nil,
        }
    }
}
