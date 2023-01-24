use std::cell::{Cell, RefCell};
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;

use intrusive_collections::{intrusive_adapter, LinkedListLink};

use crate::chunk::Instruction;
use crate::object::{BoundMethod, Class, Closure, Instance, NativeFunction, UpvalueState};
use crate::vm::error::RuntimeError;
use crate::Function;

#[derive(Clone, PartialEq)]
pub(super) enum StackValue {
    Num(f64),
    Bool(bool),

    // This is not idiomatic, but for the sake of following the book this is fine for now.
    // Plus doing this as reference means there will be a sea of lifetime indicators in here.
    // This probably should be refactored out into its own RefCell-ish type, but this will suffice
    // for now.
    // They can be strong pointers for the time being because the stack is a vector and values
    // will be dropped according as long as they go out of scope. It will probably not be possible
    // to create a cycle with classes since class will own the weak pointers instead.
    Obj(Rc<Object>),
    Nil,
}

impl Debug for StackValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            StackValue::Num(val) => write!(f, "{}", val),
            StackValue::Bool(val) => write!(f, "{}", val),
            StackValue::Obj(val) => write!(f, "{:?} -> {:?}", val, val.value),
            StackValue::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub(super) enum HeapValue {
    Str(String),
    Closure(Closure),
    NativeFunction(NativeFunction),

    // Several reasons why a interior mutability is needed here. The set_upvalue instruction
    // mutate the value this holds with whatever value is currently on top of the stack. And
    // when closed, the value needs to be updated with captured upvalue.
    // This could be replaced with a Cell if I start using pointers in StackValue and make them
    // Copy.
    Upvalue(RefCell<UpvalueState>),

    Class(Class),
    Instance(Instance),
    BoundMethod(BoundMethod),
}

impl Display for HeapValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            HeapValue::Str(val) => write!(f, "{}", val),
            HeapValue::Closure(val) => write!(f, "<Function {}>", val.fun.name),
            HeapValue::NativeFunction(val) => write!(f, "<NativeFunction {}>", val.name),
            HeapValue::Class(cls) => write!(f, "<Class {}>", cls.name),
            HeapValue::Instance(ins) => write!(f, "<Instance {}.class>", ins.class().name),
            HeapValue::BoundMethod(method) => write!(f, "{}", method.method.value),
            HeapValue::Upvalue(_) => panic!("Upvalues should not surface to users"),
        }
    }
}

#[derive(Debug)]
pub(super) struct Object {
    pub(super) heap_link: LinkedListLink,
    pub(super) upvalue_link: LinkedListLink,
    pub(super) value: HeapValue,
    pub(super) marked: Cell<bool>,
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

intrusive_adapter!(pub(super) HeapListAdapter = Rc<Object>: Object { heap_link: LinkedListLink });
intrusive_adapter!(pub(super) UpvalueListAdapter = Rc<Object>: Object { upvalue_link: LinkedListLink });

impl Object {
    pub(super) fn new(val: HeapValue) -> Rc<Object> {
        Rc::new(Object {
            heap_link: LinkedListLink::new(),
            upvalue_link: LinkedListLink::new(),
            value: val,
            marked: Cell::new(false),
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
            HeapValue::BoundMethod(method) => method.closure().fun,
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
