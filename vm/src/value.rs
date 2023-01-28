use std::fmt::{Display, Formatter};
use std::ops::{Add, Div, Mul, Neg, Not, Sub};

use crate::hashed::Hashed;
use losk_core::Literal;

use crate::object::Function;

// These values here are emitted from the source code into the chunk by the compiler.
// The chunk store them in a constant pool which are referred by their index when the vm executes
// the bytecode (which is the chunk).
#[derive(Clone, Debug, PartialEq)]
pub(crate) enum ConstantValue {
    Double(f64),
    Bool(bool),
    Str(Hashed<String>),
    Fun(Function),
    Nil,
}

impl Display for ConstantValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstantValue::Double(val) => write!(f, "{}", val),
            ConstantValue::Bool(val) => write!(f, "{}", val),
            ConstantValue::Str(val) => write!(f, "{}", val.val),
            ConstantValue::Fun(fun) => write!(f, "<Function {}>", fun.name),
            ConstantValue::Nil => write!(f, "nil"),
        }
    }
}

impl Add for ConstantValue {
    type Output = Result<ConstantValue, &'static str>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ConstantValue::Double(lhs), ConstantValue::Double(rhs)) => {
                Ok(ConstantValue::Double(lhs + rhs))
            }
            (_, _) => Err("Expect string or number operands for both."),
        }
    }
}

impl Sub for ConstantValue {
    type Output = Result<ConstantValue, &'static str>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ConstantValue::Double(lhs), ConstantValue::Double(rhs)) => {
                Ok(ConstantValue::Double(lhs - rhs))
            }
            (_, _) => Err("Expect string or number operands for both."),
        }
    }
}

impl Mul for ConstantValue {
    type Output = Result<ConstantValue, &'static str>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ConstantValue::Double(lhs), ConstantValue::Double(rhs)) => {
                Ok(ConstantValue::Double(lhs * rhs))
            }
            (_, _) => Err("Expect string or number operands for both."),
        }
    }
}

impl Div for ConstantValue {
    type Output = Result<ConstantValue, &'static str>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ConstantValue::Double(lhs), ConstantValue::Double(rhs)) => {
                Ok(ConstantValue::Double(lhs / rhs))
            }
            (_, _) => Err("Expect string or number operands for both."),
        }
    }
}

impl Neg for ConstantValue {
    type Output = Result<ConstantValue, &'static str>;

    fn neg(self) -> Self::Output {
        match self {
            ConstantValue::Double(val) => Ok(ConstantValue::Double(-val)),
            _ => Err("Expect number operand."),
        }
    }
}

impl Not for ConstantValue {
    type Output = Result<ConstantValue, &'static str>;

    fn not(self) -> Self::Output {
        match self {
            ConstantValue::Bool(val) => Ok(ConstantValue::Bool(!val)),
            _ => Err("Expect boolean operand."),
        }
    }
}

impl<'a> From<Literal> for ConstantValue {
    fn from(value: Literal) -> Self {
        match value {
            Literal::Str(val) => ConstantValue::Str(Hashed::new(val)),
            Literal::Num(val) => ConstantValue::Double(val),
            Literal::Bool(val) => ConstantValue::Bool(val),
            Literal::Nil => ConstantValue::Nil,
        }
    }
}
