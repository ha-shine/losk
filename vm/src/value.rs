use losk_core::Literal;
use std::fmt::{Display, Formatter};
use std::ops::{Add, Div, Mul, Neg, Not, Sub};

// These values here are emitted from the source code into the chunk by the compiler.
// The chunk store them in a constant pool which are referred by their index when the vm executes
// the bytecode (which is the chunk).
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Value {
    Double(f64),
    Bool(bool),
    Str(String),
    Nil,
}

impl Value {
    pub(crate) fn greater(self, other: Self) -> Result<Value, &'static str> {
        match (self, other) {
            (Value::Double(lhs), Value::Double(rhs)) => Ok(Value::Bool(lhs > rhs)),
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(Value::Bool(lhs & !rhs)),
            (Value::Nil, Value::Nil) => Ok(Value::Bool(false)),
            (_, _) => Err("Expect the operands to be of same type."),
        }
    }

    pub(crate) fn less(self, other: Self) -> Result<Value, &'static str> {
        match (self, other) {
            (Value::Double(lhs), Value::Double(rhs)) => Ok(Value::Bool(lhs < rhs)),
            (Value::Bool(lhs), Value::Bool(rhs)) => Ok(Value::Bool(!lhs & rhs)),
            (Value::Nil, Value::Nil) => Ok(Value::Bool(false)),
            (_, _) => Err("Expect the operands to be of same type."),
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Double(val) => write!(f, "{}", val),
            Value::Bool(val) => write!(f, "{}", val),
            Value::Str(val) => write!(f, "{}", val),
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

impl<'a> From<Literal> for Value {
    fn from(value: Literal) -> Self {
        match value {
            Literal::Str(val) => Value::Str(val),
            Literal::Num(val) => Value::Double(val),
            Literal::Bool(val) => Value::Bool(val),
            Literal::Nil => Value::Nil,
        }
    }
}
