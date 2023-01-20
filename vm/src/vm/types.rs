use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::rc::{Rc, Weak};

use intrusive_collections::{intrusive_adapter, LinkedListLink};

use crate::chunk::Instruction;
use crate::object::{Closure, NativeFunction};
use crate::vm::error::RuntimeError;
use crate::Function;

#[derive(Clone)]
pub(super) enum StackValue {
    Num(f64),
    Bool(bool),

    // This is not idiomatic, but for the sake of following the book this is fine for now.
    // Plus doing this as reference means there will be a sea of lifetime indicators in here.
    // This probably should be refactored out into its own RefCell-ish type, but this will suffice
    // for now.
    // The weak pointers are unwrapped because they will always point to the un-dropped RC
    // which resides in the VM's heap store.
    Obj(Weak<Object>),
    Nil,
}

impl PartialEq for StackValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (StackValue::Num(l), StackValue::Num(r)) => l == r,
            (StackValue::Bool(l), StackValue::Bool(r)) => l == r,
            (StackValue::Obj(l), StackValue::Obj(r)) => {
                l.upgrade().unwrap() == r.upgrade().unwrap()
            }
            (StackValue::Nil, StackValue::Nil) => true,
            _ => false,
        }
    }
}

impl Display for StackValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            StackValue::Num(val) => write!(f, "{}", val),
            StackValue::Bool(val) => write!(f, "{}", val),
            StackValue::Obj(val) => write!(f, "{}", val.upgrade().unwrap().value),
            StackValue::Nil => write!(f, "nil"),
        }
    }
}

impl Debug for StackValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            StackValue::Num(val) => write!(f, "{}", val),
            StackValue::Bool(val) => write!(f, "{}", val),
            StackValue::Obj(val) => write!(f, "{:?} -> {}", val, val.upgrade().unwrap().value),
            StackValue::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(super) enum HeapValue {
    Str(String),
    Closure(Closure),
    NativeFunction(NativeFunction),
}

impl HeapValue {
    pub(super) fn str(str: String) -> HeapValue {
        HeapValue::Str(str)
    }

    pub(super) fn closure(closure: Closure) -> HeapValue {
        HeapValue::Closure(closure)
    }

    pub(super) fn native(fun: NativeFunction) -> HeapValue {
        HeapValue::NativeFunction(fun)
    }
}

impl Display for HeapValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            HeapValue::Str(val) => write!(f, "{}", val),
            HeapValue::NativeFunction(fun) => write!(f, "<NativeFunction {}>", fun.name),
            HeapValue::Closure(closure) => write!(f, "<Function {}>", closure.fun.name),
        }
    }
}

#[derive(Debug)]
pub(super) struct Object {
    pub(super) link: LinkedListLink,
    pub(super) value: HeapValue,
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

intrusive_adapter!(pub(super) ListAdapter = Rc<Object>: Object { link: LinkedListLink });

impl Object {
    pub(super) fn new(val: HeapValue) -> Rc<Object> {
        Rc::new(Object {
            link: LinkedListLink::new(),
            value: val,
        })
    }
}

// CallFrame represents a frame of function that is pushed onto the call stack everytime a function
// is called. This keeps an offset of value stack pointer in `slots` which marks the start of
// this call and every value this frame owns needs to be referenced using index `slots + offset`.
// Copy-derived to initialise an array of frames with default values when the VM starts.
#[derive(Clone)]
pub(super) struct CallFrame {
    pub(super) fun: Rc<Object>,

    pub(super) ip: usize,
    pub(super) slots: usize,
}

impl CallFrame {
    pub(super) fn next(&mut self) -> Option<&Instruction> {
        self.ip += 1;
        let result = self.function().chunk.get_instruction(self.ip - 1);
        result
    }

    pub(super) fn function(&self) -> &'static Function {
        match &self.fun.value {
            HeapValue::Closure(closure) => closure.fun,
            _ => panic!("Unreachable"),
        }
    }

    pub(super) fn jump(&mut self, offset: usize) {
        self.ip += offset;
    }

    pub(super) fn loop_(&mut self, offset: usize) {
        self.ip -= offset;
    }
}

impl Default for CallFrame {
    fn default() -> Self {
        CallFrame {
            fun: Object::new(HeapValue::Str(String::new())),
            ip: 0,
            slots: 0,
        }
    }
}

pub(super) enum StackOrHeap {
    Stack(StackValue),
    Heap(HeapValue),
}

pub(super) type OpResult = Result<StackOrHeap, &'static str>;
pub(super) type VmResult<T> = Result<T, Box<RuntimeError>>;
