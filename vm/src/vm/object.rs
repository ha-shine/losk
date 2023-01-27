use ahash::RandomState;
use std::collections::HashMap;
use std::fmt::{Debug, Formatter};

use crate::chunk::{Chunk, UpvalueIndex};
use crate::limits::COMP_UPVALUE_LIMIT;
use crate::unsafe_ref::UnsafeRef;
use crate::value::ConstantValue;
use crate::vm::types::{HeapValue, Object};
use crate::vm::StackValue;

pub(super) type NativeFn = fn(&[StackValue]) -> Option<NativeValue>;

#[allow(dead_code)]
pub(super) enum NativeValue {
    Num(f64),
    Bool(bool),
    Str(String),
    Nil,
}

impl From<NativeValue> for ConstantValue {
    fn from(value: NativeValue) -> Self {
        match value {
            NativeValue::Num(val) => ConstantValue::Double(val),
            NativeValue::Bool(val) => ConstantValue::Bool(val),
            NativeValue::Str(val) => ConstantValue::Str(val),
            NativeValue::Nil => ConstantValue::Nil,
        }
    }
}

pub(super) struct NativeFunction {
    pub(super) name: &'static str,
    pub(super) arity: usize,
    pub(super) fun: NativeFn,
}

impl Debug for NativeFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "<NativeFunction {}:{}>", self.name, self.arity)
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.arity == other.arity
    }
}

#[derive(Copy, Clone, Debug, Default, PartialEq)]
pub(crate) struct Upvalue {
    pub(crate) index: usize,
    pub(crate) is_local: bool,
}

impl Upvalue {
    pub(crate) fn new(index: usize, is_local: bool) -> Self {
        Upvalue { index, is_local }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub(crate) name: String,
    pub(crate) arity: usize,
    pub(crate) chunk: Chunk,
    pub(crate) upvalue_count: usize,
    pub(crate) upvalues: [Upvalue; COMP_UPVALUE_LIMIT],
}

impl Function {
    pub(crate) fn new(name: String, arity: usize) -> Self {
        Function {
            name,
            arity,
            chunk: Chunk::new(),
            upvalues: [Default::default(); COMP_UPVALUE_LIMIT],
            upvalue_count: 0,
        }
    }

    pub(crate) fn add_upvalue(&mut self, upvalue: Upvalue) -> Result<UpvalueIndex, &'static str> {
        let idx = self.upvalue_count;

        // First check if the same captured variable has been referenced before
        if let Some(i) = (0..idx).rev().find(|i| {
            self.upvalues[*i].index == upvalue.index
                && self.upvalues[*i].is_local == upvalue.is_local
        }) {
            Ok(UpvalueIndex(i))
        } else if idx == COMP_UPVALUE_LIMIT {
            Err("Too many closure variables in a function.")
        } else {
            self.upvalue_count += 1;
            self.upvalues[idx] = upvalue;
            Ok(UpvalueIndex(idx))
        }
    }
}

impl NativeFunction {
    pub(crate) fn new(
        name: &'static str,
        arity: usize,
        fun: fn(&[StackValue]) -> Option<NativeValue>,
    ) -> Self {
        NativeFunction { name, arity, fun }
    }
}

// Closure contains upvalues and upvalue count through the function pointer, so captured can be
// created with known size in advance.
// Some primer on upvalues, closed upvalues are those which have been captured by the closure, i.e
// their ownership is transferred from their original scope to the closure because the original
// scope will go out and the upvalue would be dropped. The opened upvalues are those which are
// still referring to the original value through stack offsets. Note that in the textbook, they
// are pointers. But I will need to use an index because of Rust.
#[derive(Debug)]
pub(super) struct Closure {
    pub(super) fun: &'static Function,

    // This should be a vector of pointers to upvalue objects (which contains stack position).
    // The VM will hold onto a bunch of opened upvalue objects in a separate linked list that is
    // different from the current heap (and the linked list will be sorted by the stack position).
    // Whenever the close upvalue instruction is executed, some of those upvalues will be dropped
    // from the linked list and be updated with pointer to newly hoisted upvalue on the heap.
    // Also whenever the callframe is popped, every open upvalues with index higher than the
    // callframe's slots are removed.
    //
    // Should these be weak pointers?
    // So I can use enum for these - Open(StackPosition), Closed(StackValue)
    // Multiple upvalues referring to the same stack position will point to the same object.
    // I'm not sure if it's possible to create a cycle through upvalues because they are using Rc.
    // Theoretically, an upvalue could point another object which contains a field to upvalue, though
    // I haven't thought about the problem thoroughly.
    pub(super) captured: Vec<UnsafeRef<Object>>,
}

impl PartialEq for Closure {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

#[derive(Debug, PartialEq)]
pub(super) enum UpvalueState {
    Open(*mut StackValue),
    Closed(StackValue),
}

#[derive(Debug, PartialEq)]
pub(super) struct Class {
    pub(super) name: &'static str,
    pub(super) methods: HashMap<&'static str, StackValue, RandomState>,
}

impl Class {
    pub(super) fn new(name: &'static str) -> Class {
        Class {
            name,
            methods: HashMap::default(),
        }
    }
}

#[derive(Debug)]
pub(super) struct Instance {
    pub(super) class: UnsafeRef<Object>,

    // RefCell is required here because I need to set the properties
    pub(super) fields: HashMap<&'static str, StackValue, RandomState>,
}

impl Instance {
    pub(super) fn new(class: UnsafeRef<Object>) -> Instance {
        Instance {
            class,
            fields: HashMap::default(),
        }
    }

    pub(super) fn class(&self) -> &Class {
        match &self.class.value {
            HeapValue::Class(class) => class,
            _ => panic!("Unreachable"),
        }
    }
}

impl PartialEq for Instance {
    fn eq(&self, other: &Self) -> bool {
        if !std::ptr::eq(&self.class, &other.class) || self.fields.len() != other.fields.len() {
            return false;
        }

        for (k, v) in self.fields.iter() {
            match other.fields.get(k) {
                Some(val) if v == val => { /* Values are equal, continue */ }
                _ => return false,
            };
        }

        true
    }
}

#[derive(Debug)]
pub(super) struct BoundMethod {
    pub(super) receiver: StackValue,
    pub(super) method: UnsafeRef<Object>,
}

impl PartialEq for BoundMethod {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self, other)
    }
}

impl BoundMethod {
    pub(super) fn new(receiver: StackValue, method: UnsafeRef<Object>) -> Self {
        BoundMethod { receiver, method }
    }

    pub(super) fn instance(&self) -> &Instance {
        let obj = match &self.receiver {
            StackValue::Obj(obj) => obj,
            _ => panic!("Unreachable"),
        };

        match &obj.value {
            HeapValue::Instance(instance) => instance,
            _ => panic!("Unreachable"),
        }
    }

    pub(super) fn closure(&self) -> &Closure {
        match &self.method.value {
            HeapValue::Closure(closure) => closure,
            _ => panic!("Unreachable"),
        }
    }
}
