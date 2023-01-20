use std::cell::RefCell;
use std::fmt::{Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ptr;
use std::rc::Rc;

use losk_core::Literal;

use crate::callable::{Callable, Instance};

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Callable(Rc<dyn Callable>),
    Instance(Rc<RefCell<Instance>>),
    Str(Rc<String>),
    Num(f64),
    Bool(bool),
    Nil,
}

impl From<Literal> for Value {
    fn from(value: Literal) -> Self {
        match value {
            Literal::Str(val) => Value::Str(Rc::new(val)),
            Literal::Num(val) => Value::Num(val),
            Literal::Bool(val) => Value::Bool(val),
            Literal::Nil => Value::Nil,
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Callable(lhs), Value::Callable(rhs)) => ptr::eq(lhs, rhs),
            (Value::Str(lhs), Value::Str(rhs)) => lhs == rhs,
            (Value::Num(lhs), Value::Num(rhs)) => lhs == rhs,
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Nil, Value::Nil) => true,
            (Value::Nil, _) => false,
            (_, Value::Nil) => false,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Callable(ptr) => Rc::as_ptr(ptr).hash(state),
            Value::Instance(ptr) => Rc::as_ptr(ptr).hash(state),
            Value::Str(val) => val.hash(state),
            Value::Num(val) => val.to_bits().hash(state),
            Value::Bool(val) => val.hash(state),
            Value::Nil => 0.hash(state),
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Value::Bool(value)
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Value::Str(Rc::new(value))
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Value::Str(Rc::new(String::from(value)))
    }
}

macro_rules! impl_from_num_for_value {
    ( $( $t:ident )* ) => {
        $(
            impl From<$t> for Value {
                fn from(n: $t) -> Value {
                    Value::Num(n as f64)
                }
            }
        )*
    }
}

impl_from_num_for_value!(u8 i8 u16 i16 u32 i32 u64 i64 u128 i128 usize isize f32 f64);

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Callable(val) => {
                write!(f, "{:?}", val)
            }
            Value::Instance(instance) => {
                write!(f, "{}", RefCell::borrow(instance))
            }
            Value::Str(val) => write!(f, "{}", val),
            Value::Num(val) => write!(f, "{}", val),
            Value::Bool(val) => write!(f, "{}", val),
            Value::Nil => write!(f, "nil"),
        }
    }
}
