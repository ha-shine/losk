use crate::chunk::Instruction;
use crate::object::NativeFunction;
use crate::vm::error::RuntimeError;
use crate::Function;
use intrusive_collections::{intrusive_adapter, LinkedListLink};
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Deref;
use std::rc::Rc;

#[derive(Clone, PartialEq)]
pub(super) enum StackValue {
    Num(f64),
    Bool(bool),

    // This is not idiomatic, but for the sake of following the book this is fine for now.
    // Plus doing this as reference means there will be a sea of lifetime indicators in here.
    // This probably should be refactored out into its own RefCell-ish type, but this will suffice
    // for now.
    Obj(Rc<Object>),
    Nil,
}

impl Display for StackValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            StackValue::Num(val) => write!(f, "{}", val),
            StackValue::Bool(val) => write!(f, "{}", val),
            StackValue::Obj(val) => write!(f, "{}", val.value),
            StackValue::Nil => write!(f, "nil"),
        }
    }
}

impl Debug for StackValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            StackValue::Num(val) => write!(f, "{}", val),
            StackValue::Bool(val) => write!(f, "{}", val),
            StackValue::Obj(val) => write!(f, "{:?} -> {}", val, val.value),
            StackValue::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(super) enum HeapValue {
    Str(String),
    Fun(Function),
    NativeFunction(NativeFunction),
    Nil,
}

impl Display for HeapValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            HeapValue::Str(val) => write!(f, "{}", val),
            HeapValue::Fun(fun) => write!(f, "<Function {}>", fun.name),
            HeapValue::NativeFunction(fun) => write!(f, "<NativeFunction {}>", fun.name),
            HeapValue::Nil => write!(f, "Nil"),
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
    // Unsafe pointer is used for performance, but the actual function will reside in the VM's
    // main function
    pub(super) fun: Rc<Object>,

    pub(super) ip: usize,
    pub(super) slots: usize,
}

impl CallFrame {
    pub(super) fn next(&mut self) -> Option<&Instruction> {
        self.ip += 1;
        let result = self.fun().chunk.get_instruction(self.ip - 1);
        result
    }

    pub(super) fn fun(&self) -> &Function {
        match &self.fun.value {
            HeapValue::Fun(fun) => fun,
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
            fun: Object::new(HeapValue::Nil),
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
