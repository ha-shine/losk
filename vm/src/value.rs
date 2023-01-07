use core::Literal;
use std::fmt::{Display, Formatter};
use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Copy, Clone, Debug)]
pub(crate) enum Value {
    Double(f64),
    Empty,
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Double(val) => write!(f, "{}", val),
            Value::Empty => write!(f, "nil"),
        }
    }
}

impl Add for Value {
    type Output = Value;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Double(lhs), Value::Double(rhs)) => Value::Double(lhs + rhs),
            (_, _) => Value::Empty,
        }
    }
}

impl Sub for Value {
    type Output = Value;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Double(lhs), Value::Double(rhs)) => Value::Double(lhs - rhs),
            (_, _) => Value::Empty,
        }
    }
}

impl Mul for Value {
    type Output = Value;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Double(lhs), Value::Double(rhs)) => Value::Double(lhs * rhs),
            (_, _) => Value::Empty,
        }
    }
}

impl Div for Value {
    type Output = Value;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Value::Double(lhs), Value::Double(rhs)) => Value::Double(lhs / rhs),
            (_, _) => Value::Empty,
        }
    }
}

impl Neg for Value {
    type Output = Self;

    fn neg(self) -> Self::Output {
        match self {
            Value::Double(val) => Value::Double(-val),
            _ => Value::Empty,
        }
    }
}

impl From<Literal> for Value {
    fn from(value: Literal) -> Self {
        match value {
            Literal::Str(_) => Value::Empty,
            Literal::Num(val) => Value::Double(val),
            Literal::Bool(_) => Value::Empty,
            Literal::Nil => Value::Empty,
        }
    }
}
