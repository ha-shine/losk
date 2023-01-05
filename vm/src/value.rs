use std::fmt::{Display, Formatter};

#[derive(Copy, Clone)]
pub(crate) enum Value {
    Double(Double),
    Empty,
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Double(val) => write!(f, "{}", val.0),
            Value::Empty => write!(f, "nil"),
        }
    }
}

#[derive(Copy, Clone)]
pub(crate) struct Double(pub(crate) f64);
