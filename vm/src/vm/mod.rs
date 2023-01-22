use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use std::io::Write;
use std::rc::Rc;

use intrusive_collections::LinkedList;

use native::clock;

use crate::chunk::*;
use crate::object::{Closure, Function, NativeFunction, NativeValue, UpvalueState};
use crate::value::ConstantValue;
use crate::vm::error::*;
use crate::vm::types::*;

mod error;
pub mod native;
pub mod object;
mod types;

const DEFAULT_STACK: usize = 256;
const FRAMES_MAX: usize = 256;

pub struct VM<'a> {
    // The stack as a growable vector. In textbook, this is represented by an array and a stack
    // pointer that always point to one element past the top. This is not required here since the
    // vector already give us those functionality.
    stack: Vec<StackValue>,

    // List of objects currently allocated on the heap
    // Note that the heap objects need to be mutable because of two reasons -
    //   - Garbage collector will mark the objects
    //   - Closure's captured values will be updated with `SetUpvalue` instruction
    objects: LinkedList<HeapListAdapter>,

    // The call frame is limited and is allocated on the rust stack for faster access compared to
    // a heap. This comes with limitation that the frame count will be limited and pre-allocation
    // when the VM starts.
    frames: Vec<CallFrame>,

    // List of upvalues that have not been closed. They are objects here because there's not a good
    // way to point directly to the UpvalueObject unless I go the route of double indirection
    // by wrapping the UpvalueObject itself in an Rc.
    open_upvalues: LinkedList<UpvalueListAdapter>,

    // Globals are used to map between a variable defined on the global scope to it's corresponding
    // value. The values are of type `StackValue` because the globals can be mapped to either stack
    // or heap values. The expression after the var declaration statement is pushed onto the stack
    // so it makes sense to map to a `StackValue`. Care should be taken so that the GC scans this
    // table for reachability too.
    globals: HashMap<String, StackValue>,

    // The VM will write the output strings into this stdout
    stdout: &'a mut dyn Write,
}

// Be aware the VM will leak the function passed into to create a static reference to it and
// it will not be reclaimed
impl<'a> VM<'a> {
    pub fn new(stdout: &'a mut dyn Write) -> Self {
        // In the
        let mut vm = VM {
            stack: Vec::with_capacity(DEFAULT_STACK),
            objects: LinkedList::new(HeapListAdapter::new()),
            frames: Vec::with_capacity(FRAMES_MAX),
            open_upvalues: LinkedList::new(UpvalueListAdapter::new()),
            globals: HashMap::new(),
            stdout,
        };

        let clock = Object::new(HeapValue::native(NativeFunction::new("clock", 0, clock)));
        vm.objects.push_front(clock.clone());
        vm.globals
            .insert("clock".to_string(), StackValue::Obj(Rc::downgrade(&clock)));
        vm
    }

    fn make_closure(&mut self, fun: &'static Function) -> Closure {
        let mut captured = Vec::with_capacity(fun.upvalue_count);
        if fun.upvalue_count == 0 {
            return Closure { fun, captured };
        }

        // Early return took care of the main <script>, so frame count will be definitely more than
        // 1. And the parent always is a closure because it's interpreting a closure instruction.
        let p_frame = self.current_frame();
        let slots = p_frame.slots;
        let p_obj = &p_frame.fun;
        let p_closure = match &p_obj.value {
            HeapValue::Closure(closure) => closure,
            _ => panic!("Unreachable"),
        };

        // Another workaround for Rust's borrow checker. p_closure is self's field and it's been
        // borrowed immutably, but `capture_upvalue` requires a mutable borrow to create a new
        // upvalue object and add it to the upvalue list. This can't be done in this because
        // the `capture_upvalue` do other heavy-lifting stuffs like moving the cursor to correct place.
        enum UpvalueOrStackPosition {
            Upvalue(Rc<Object>),
            Position(StackPosition),
        }

        // A bit disappointing that I need a temporary vector just for this. If this proves to be
        // too much of a hit on performance, I can always copy the code inline.
        let temp: Vec<_> = (0..fun.upvalue_count)
            .map(|idx| fun.upvalues[idx])
            .map(|upvalue| {
                if upvalue.is_local {
                    UpvalueOrStackPosition::Position(StackPosition::Index(slots + upvalue.index))
                } else {
                    UpvalueOrStackPosition::Upvalue(p_closure.captured[upvalue.index].clone())
                }
            })
            .collect();

        for value in temp {
            match value {
                UpvalueOrStackPosition::Upvalue(val) => captured.push(val),
                UpvalueOrStackPosition::Position(pos) => captured.push(self.capture_upvalue(pos)),
            }
        }

        Closure { fun, captured }
    }

    // Once I switch over to using pointers, I can easily assume the main function will be
    // passed by a reference and hence will be valid for the entirety of this `run` function.
    // All the functions can be read with a pointer in that case.
    pub fn run(&mut self, main: Function) -> VmResult<()> {
        let leaked = Box::leak(Box::new(main));
        let main_closure = self.make_closure(leaked);
        let main = Object::new(HeapValue::closure(main_closure));
        self.objects.push_front(main.clone());
        self.stack.push(StackValue::Obj(Rc::downgrade(&main)));
        self.frames.push(CallFrame {
            fun: main,
            ip: 0,
            slots: 0,
        });

        self.interpret()
    }

    fn interpret(&mut self) -> VmResult<()> {
        loop {
            // Instruction is cheap to copy, though the run loop is very sensitive to performance
            // and not sure this would affect the runtime severely.
            let instruction = match self.current_frame_mut().next() {
                Some(instruction) => *instruction,
                None => return Ok(()),
            };

            #[cfg(feature = "debug-trace-execution")]
            for val in &self.stack {
                writeln!(self.stdout, "[ {:10?} ]", val).unwrap();
            }

            match instruction {
                Instruction::Constant(val) => self.execute_constant(val)?,
                Instruction::LiteralTrue => self.push(StackValue::Bool(true)),
                Instruction::LiteralFalse => self.push(StackValue::Bool(false)),
                Instruction::LiteralNil => self.push(StackValue::Nil),
                Instruction::Pop => {
                    self.pop();
                }
                Instruction::GetGlobal(Constant(index)) => {
                    let name = match self
                        .current_frame()
                        .function()
                        .chunk
                        .get_constant(index as usize)
                    {
                        Some(ConstantValue::Str(name)) => name,
                        _ => panic!("Unreachable"),
                    };

                    match self.globals.get(name) {
                        Some(sval) => self.push(sval.clone()),
                        None => {
                            return Err(self.error(format_args!("Undefined variable '{}'.", name)))
                        }
                    }
                }
                Instruction::DefineGlobal(Constant(index)) => {
                    // The global name is stored as a constant value in the constant pool, read
                    // from the pool to get the name.
                    let name = match self
                        .current_frame()
                        .function()
                        .chunk
                        .get_constant(index as usize)
                    {
                        Some(ConstantValue::Str(val)) => val.clone(),
                        _ => panic!("Unreachable"),
                    };

                    let val = self.pop();
                    self.globals.insert(name.clone(), val);
                }
                Instruction::SetGlobal(val) => self.execute_set_global(val)?,

                Instruction::GetLocal(pos) => {
                    let val = self.peek_stack(pos).clone();
                    self.push(val);
                }

                Instruction::SetLocal(pos) => {
                    let last = self.stack.last().unwrap().clone();
                    self.put_stack(pos, last);
                }

                Instruction::GetUpvalue(UpvalueIndex(index)) => {
                    if let HeapValue::Closure(closure) = &self.current_frame().fun.value {
                        let value = self.get_upvalue(&closure.captured[index]);
                        self.push(value);
                    } else {
                        // This means the compiler borked the compilation
                        panic!("Unreachable")
                    }
                }
                Instruction::SetUpvalue(UpvalueIndex(index)) => {
                    let top = self.peek_stack(StackPosition::RevOffset(0)).clone();
                    if let HeapValue::Closure(closure) = &self.current_frame().fun.value {
                        self.set_upvalue(closure.captured[index].clone(), top);
                    } else {
                        panic!("Unreachable")
                    }
                }
                Instruction::Equal => {
                    let rhs = self.pop();
                    let lhs = self.pop();
                    self.push(StackValue::Bool(lhs == rhs));
                }
                Instruction::Greater => self.execute_binary_op(Self::greater)?,
                Instruction::Less => self.execute_binary_op(Self::less)?,
                Instruction::Negate => self.execute_unary_op(Self::neg)?,
                Instruction::Not => self.execute_unary_op(Self::not)?,
                Instruction::Add => self.execute_binary_op(Self::add)?,
                Instruction::Subtract => self.execute_binary_op(Self::sub)?,
                Instruction::Multiply => self.execute_binary_op(Self::mul)?,
                Instruction::Divide => self.execute_binary_op(Self::div)?,
                Instruction::Print => {
                    let val = self.pop();
                    self.print_stack_value(val);
                }
                Instruction::JumpIfFalse(JumpDist(dist)) => {
                    // The pointer has already been incremented by 1 before this match statement,
                    // so need to subtract 1 from jump distance.
                    match self.peek_stack(StackPosition::RevOffset(0)) {
                        StackValue::Bool(val) => {
                            if !val {
                                self.current_frame_mut().jump(dist - 1)
                            }
                        }
                        _ => {
                            return Err(
                                self.error(format_args!("Expect test condition to be boolean."))
                            )
                        }
                    }
                }
                Instruction::Jump(JumpDist(dist)) => {
                    // Same ordeal as above here.
                    self.current_frame_mut().jump(dist - 1)
                }
                Instruction::Loop(JumpDist(dist)) => {
                    // Add 1 to distance because the instruction pointer has already been incremented
                    // by 1, so it's pointing to the instruction one after this.
                    self.current_frame_mut().loop_(dist + 1)
                }
                Instruction::Call(ArgCount(count)) => {
                    if self.frames.len() == FRAMES_MAX {
                        return Err(self.error(format_args!("Stack overflow.")));
                    }

                    // The previous instructions must have pushed `count` amount to stack, so
                    // the function value would exist at stack[-count].
                    if let StackValue::Obj(obj) = self.peek_stack(StackPosition::RevOffset(count)) {
                        let obj = obj.upgrade().unwrap();
                        match &obj.value {
                            HeapValue::Closure(closure) => {
                                if count != closure.fun.arity {
                                    return Err(self.error(format_args!(
                                        "Expected {} arguments but got {}.",
                                        closure.fun.arity, count
                                    )));
                                }

                                self.frames.push(CallFrame {
                                    fun: obj,
                                    ip: 0,
                                    slots: self.stack.len() - count - 1,
                                });
                            }
                            HeapValue::NativeFunction(fun) => {
                                if count != fun.arity {
                                    return Err(self.error(format_args!(
                                        "Expected {} arguments but got {}.",
                                        fun.arity, count
                                    )));
                                }

                                let arg_beg = self.stack.len() - count;
                                let result = (fun.fun)(&self.stack[arg_beg..]);
                                self.pop_n(count + 1); // args + function data
                                if let Some(result) = result {
                                    self.store_native_value(result)?;
                                }
                            }
                            _ => {
                                return Err(
                                    self.error(format_args!("Can only call functions and classes"))
                                )
                            }
                        }
                    } else {
                        return Err(self.error(format_args!("Can only call functions and classes")));
                    }
                }

                // Maybe a clearer way is to separate the input and output type of constant.
                // Even though functions are constant too, I think it would be more readable if the
                // chunk stores the constants internally as the same value but have two separate
                // methods with different return types for normal constant values and functions.
                // And also maybe use Box<Function> for functions since that's the heaviest
                // variant in the ConstantValue enum.
                // I can use two containers for functions and constant values, that would make
                // compilation a bit more complicated and might not be a good thing for performance
                // due to cache locality (or lack thereof) between the two containers.
                Instruction::Closure(Constant(idx)) => {
                    let val = self
                        .current_frame()
                        .function()
                        .chunk
                        .get_constant(idx as usize);

                    if let Some(ConstantValue::Fun(fun)) = val {
                        let closure = self.make_closure(fun);
                        self.allocate_and_push(HeapValue::Closure(closure));
                    } else {
                        panic!("Unreachable")
                    }
                }
                Instruction::CloseUpvalue => {
                    self.close_upvalues(self.stack.len() - 1);
                    self.pop();
                }
                Instruction::Return => {
                    // Ensure the current frame's closure is dropped after returning.
                    let frame = self.frames.pop().unwrap();

                    // The return value of a function should be on top of the stack, so it'll be
                    // pushed onto the top of the stack.
                    let result = self.pop();
                    let slots = frame.slots;
                    self.close_upvalues(slots);

                    if self.frames.is_empty() {
                        self.pop();
                        return Ok(());
                    }

                    // After calling a function, the stack top needs to point to the place where
                    // the function information is stored previously (so frame.slots).
                    // To get back there, I need to pop stack.len() - slots.
                    self.pop_n(self.stack.len() - slots);
                    self.push(result);
                }
            }
        }
    }

    fn execute_set_global(&mut self, constant: Constant) -> VmResult<()> {
        // Note that the set variable doesn't pop the value from the stack because the set assignment
        // is an expression statement and there always is a pop instruction after expression statement.
        let val = self.peek_stack(StackPosition::RevOffset(0)).clone();

        // It's a bit wasteful to clone the constant name everytime it's assigned, but I want to
        // avoid unsafe code in this before profiling.
        let name = match self
            .current_frame()
            .function()
            .chunk
            .get_constant(constant.0 as usize)
        {
            Some(ConstantValue::Str(name)) => name.clone(),
            _ => panic!("Unreachable"),
        };

        match self.globals.get_mut(&name) {
            Some(entry) => {
                *entry = val;
                Ok(())
            }
            None => Err(self.error(format_args!("Undefined variable '{}'.", name))),
        }
    }

    fn execute_constant(&mut self, con: Constant) -> VmResult<()> {
        let constant = self
            .current_frame()
            .function()
            .chunk
            .get_constant(con.0 as usize)
            .ok_or_else(|| self.error(format_args!("Unknown constant")))?;

        match constant {
            ConstantValue::Double(val) => self.push(StackValue::Num(*val)),
            ConstantValue::Bool(val) => self.push(StackValue::Bool(*val)),
            ConstantValue::Str(val) => self.allocate_and_push(HeapValue::str(val.clone())),
            ConstantValue::Nil => self.push(StackValue::Nil),
            ConstantValue::Fun(_) => panic!("Unreachable"),
        }

        Ok(())
    }

    fn execute_unary_op(&mut self, op: fn(&mut Self, StackValue) -> OpResult) -> VmResult<()> {
        let val = self.pop();
        let res = op(self, val);
        self.store_op_result(res)
    }

    fn execute_binary_op(
        &mut self,
        op: fn(&mut Self, StackValue, StackValue) -> OpResult,
    ) -> VmResult<()> {
        let rhs = self.pop();
        let lhs = self.pop();
        let res = op(self, lhs, rhs);
        self.store_op_result(res)
    }

    fn store_op_result(&mut self, res: OpResult) -> VmResult<()> {
        match res {
            Ok(StackOrHeap::Stack(val)) => {
                self.stack.push(val);
                Ok(())
            }
            Ok(StackOrHeap::Heap(val)) => {
                self.allocate_and_push(val);
                Ok(())
            }
            Err(msg) => Err(self.error(format_args!("{}", msg))),
        }
    }

    // These operators mean that the GC should be carefully considered.
    // Say you're adding two strings, the result string will be allocated on the heap
    // that is the concatenation of the two operands.
    // During the time between the result string has been allocated and before the value (pointer)
    // is saved to stack, that string will not be reachable from the stack. If the GC runs during
    // that time, the result could be gc-ed and the stack value will point to no where.
    // Care needs to be taken when adding values to heap.
    fn add(&mut self, lhs: StackValue, rhs: StackValue) -> OpResult {
        match (lhs, rhs) {
            (StackValue::Num(l), StackValue::Num(r)) => {
                Ok(StackOrHeap::Stack(StackValue::Num(l + r)))
            }
            (StackValue::Obj(l), StackValue::Obj(r)) => {
                match (&l.upgrade().unwrap().value, &r.upgrade().unwrap().value) {
                    (HeapValue::Str(lstr), HeapValue::Str(rstr)) => {
                        let mut res = String::with_capacity(lstr.len() + rstr.len());
                        res += lstr;
                        res += rstr;
                        Ok(StackOrHeap::Heap(HeapValue::Str(res)))
                    }
                    _ => Err("Expect both operands to be either numbers or strings."),
                }
            }
            (_, _) => Err("Expect both operands to be either numbers or strings."),
        }
    }

    fn sub(&mut self, lhs: StackValue, rhs: StackValue) -> OpResult {
        match (lhs, rhs) {
            (StackValue::Num(l), StackValue::Num(r)) => {
                Ok(StackOrHeap::Stack(StackValue::Num(l - r)))
            }
            (_, _) => Err("Expect both operands to be numbers."),
        }
    }

    fn mul(&mut self, lhs: StackValue, rhs: StackValue) -> OpResult {
        match (lhs, rhs) {
            (StackValue::Num(l), StackValue::Num(r)) => {
                Ok(StackOrHeap::Stack(StackValue::Num(l * r)))
            }
            (_, _) => Err("Expect both operands to be numbers."),
        }
    }

    fn div(&mut self, lhs: StackValue, rhs: StackValue) -> OpResult {
        match (lhs, rhs) {
            (StackValue::Num(l), StackValue::Num(r)) => {
                Ok(StackOrHeap::Stack(StackValue::Num(l / r)))
            }
            (_, _) => Err("Expect both operands to be numbers."),
        }
    }

    fn greater(&mut self, lhs: StackValue, rhs: StackValue) -> OpResult {
        match (lhs, rhs) {
            (StackValue::Num(l), StackValue::Num(r)) => {
                Ok(StackOrHeap::Stack(StackValue::Bool(l > r)))
            }
            (_, _) => Err("Expect both operands to be numbers."),
        }
    }

    fn less(&mut self, lhs: StackValue, rhs: StackValue) -> OpResult {
        match (lhs, rhs) {
            (StackValue::Num(l), StackValue::Num(r)) => {
                Ok(StackOrHeap::Stack(StackValue::Bool(l < r)))
            }
            (_, _) => Err("Expect both operands to be numbers."),
        }
    }

    fn not(&mut self, val: StackValue) -> OpResult {
        match val {
            StackValue::Bool(val) => Ok(StackOrHeap::Stack(StackValue::Bool(!val))),
            _ => Err("Expect operand to be a boolean value."),
        }
    }

    fn neg(&mut self, val: StackValue) -> OpResult {
        match val {
            StackValue::Num(val) => Ok(StackOrHeap::Stack(StackValue::Num(-val))),
            _ => Err("Expect operand to be a number value."),
        }
    }

    fn store_native_value(&mut self, value: NativeValue) -> VmResult<()> {
        match ConstantValue::from(value) {
            ConstantValue::Double(val) => self.push(StackValue::Num(val)),
            ConstantValue::Bool(val) => self.push(StackValue::Bool(val)),
            ConstantValue::Str(val) => self.allocate_and_push(HeapValue::str(val)),
            ConstantValue::Nil => self.push(StackValue::Nil),
            ConstantValue::Fun(_) => {
                return Err(self.error(format_args!("Invalid return value from native function")))
            }
        }

        Ok(())
    }

    fn print_stack_value(&mut self, val: StackValue) {
        match val {
            StackValue::Num(val) => {
                writeln!(self.stdout, "{}", val).unwrap();
            }
            StackValue::Bool(val) => {
                writeln!(self.stdout, "{}", val).unwrap();
            }
            StackValue::Obj(object) => self.print_heap_value(&object.upgrade().unwrap().value),
            StackValue::Nil => {
                writeln!(self.stdout, "nil").unwrap();
            }
        };
    }

    fn print_heap_value(&mut self, val: &HeapValue) {
        match val {
            HeapValue::Str(val) => {
                writeln!(self.stdout, "{}", val).unwrap();
            }
            HeapValue::Closure(val) => {
                writeln!(self.stdout, "<Function {}>", val.fun.name).unwrap();
            }
            HeapValue::NativeFunction(val) => {
                writeln!(self.stdout, "<NativeFunction {}>", val.name).unwrap();
            }
            HeapValue::Upvalue(val) => {
                match &*val.borrow() {
                    UpvalueState::Open(pos) => {
                        self.print_stack_value(self.peek_stack(*pos).clone())
                    }
                    UpvalueState::Closed(val) => self.print_stack_value(val.clone()),
                };
            }
        };
    }

    fn capture_upvalue(&mut self, pos: StackPosition) -> Rc<Object> {
        let index = self.stack_pos_to_index(pos);
        let mut cursor = self.open_upvalues.cursor_mut();
        cursor.move_next();

        // The loop will return an Rc of object that should be inserted into the upvalues.
        // It will also ensure the cursor is always pointing to a position right next to where the
        // object go so that the object should use the cursor's `insert_before`. A second value
        // indicates whether the object is a newly created one, and if so it should be allocated
        // on the heap.
        // This is because of the restriction with Rust where `self` can't be borrowed twice,
        // once with the cursor and again in allocate_object.
        let (obj, is_new) = loop {
            // Because of more rust (and intrusive pointer) restrictions, I have to extract
            // the value of comparison out of this and do the cursor processing after this. Here,
            // `get` is borrowed immutably, but I need to remove the node (or move to the next)
            // node which requires a mutable cursor.
            let comparison = match cursor.get() {
                Some(Object {
                    value: HeapValue::Upvalue(val),
                    ..
                }) => match &*val.borrow() {
                    UpvalueState::Open(StackPosition::Index(cursor_index)) => {
                        cursor_index.cmp(&index)
                    }
                    _ => panic!("Unreachable"),
                },

                // If current is null, that means it's the end of the list. Create a new node and add
                // it to the list
                None => {
                    break (
                        Object::new(HeapValue::Upvalue(RefCell::new(UpvalueState::Open(pos)))),
                        true,
                    );
                }

                // This is a cursor for open upvalues, so it's definitely a panic to see anything
                // else. Also, upvalues are captured as purely stack indices, so anything else
                // is a runtime error.
                _ => panic!("Unreachable"),
            };

            match comparison {
                // The current cursor's index is less than the index of the upvalue being
                // captured. That means a new upvalue needs to be created, and inserted into
                // the previous place. The open upvalue objects are sorted based on their
                // stack position, from topmost value to bottommost.
                Ordering::Less => {
                    break (
                        Object::new(HeapValue::Upvalue(RefCell::new(UpvalueState::Open(pos)))),
                        true,
                    );
                }

                // Unfortunately, due to the limitation of intrusive linked list, I can't seem
                // to be able to get the reference to the container (i.e an Rc of object),
                // but only to the contained &Object. As a work around, it's necessary to remove
                // the current node, clone the removed Rc and insert it back again.
                Ordering::Equal => {
                    // Now cursor is pointing to the one after this
                    let removed = cursor.remove().unwrap();
                    break (removed, false);
                }

                Ordering::Greater => {
                    cursor.move_next();
                }
            }
        };

        cursor.insert_before(obj.clone());
        if is_new {
            self.objects.push_front(obj.clone());
        }

        obj
    }

    fn close_upvalues(&mut self, last_index: usize) {
        let mut cursor = self.open_upvalues.cursor_mut();
        cursor.move_next();

        while !cursor.is_null() {
            if let HeapValue::Upvalue(val) = &cursor.get().unwrap().value {
                let index = match &*val.borrow() {
                    UpvalueState::Open(StackPosition::Index(index)) => *index,
                    _ => panic!("Found closed upvalue in open upvalue list"),
                };

                if index > last_index {
                    break;
                }

                let item = self.stack[index].clone();
                *val.borrow_mut() = UpvalueState::Closed(item);

                cursor.remove();
            } else {
                panic!("Found non upvalue item in upvalue list")
            }
        }
    }

    // This will return the stack object the given upvalue object is pointing to, which means
    // the original stack value if it's an open upvalue, or pointer to the object if it's closed.
    // The method assumes the given object is an upvalue, and panic if not.
    fn get_upvalue(&self, obj: &Rc<Object>) -> StackValue {
        match &obj.value {
            HeapValue::Upvalue(val) => match &*val.borrow() {
                UpvalueState::Open(pos) => self.peek_stack(*pos).clone(),
                UpvalueState::Closed(_) => StackValue::Obj(Rc::downgrade(obj)),
            },
            _ => panic!("Object is not an upvalue"),
        }
    }

    fn set_upvalue(&mut self, obj: Rc<Object>, value: StackValue) {
        match &obj.value {
            HeapValue::Upvalue(val) => {
                if let UpvalueState::Open(pos) = &*val.borrow() {
                    self.put_stack(*pos, value);
                    return;
                }

                *val.borrow_mut() = UpvalueState::Closed(value);
            }
            _ => panic!("Object is not an upvalue"),
        };
    }

    fn peek_stack(&self, pos: StackPosition) -> &StackValue {
        let index = self.stack_pos_to_index(pos);
        &self.stack[index]
    }

    fn put_stack(&mut self, pos: StackPosition, val: StackValue) {
        let index = self.stack_pos_to_index(pos);
        self.stack[index] = val
    }

    fn stack_pos_to_index(&self, pos: StackPosition) -> usize {
        match pos {
            StackPosition::Offset(offset) => self.current_frame().slots + offset,
            StackPosition::Index(index) => index,
            StackPosition::RevOffset(offset) => self.stack.len() - offset - 1,
        }
    }

    fn push(&mut self, val: StackValue) {
        self.stack.push(val)
    }

    fn pop(&mut self) -> StackValue {
        self.stack.pop().unwrap()
    }

    fn pop_n(&mut self, n: usize) {
        let start = self.stack.len() - n;
        self.stack.drain(start..);
    }

    fn allocate_and_push(&mut self, val: HeapValue) {
        let hval = self.allocate_object(val);
        let sval = StackValue::Obj(Rc::downgrade(&hval));
        self.stack.push(sval);
    }

    fn allocate_object(&mut self, val: HeapValue) -> Rc<Object> {
        let value = Object::new(val);
        self.objects.push_front(value.clone());
        value
    }

    fn current_frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().unwrap()
    }

    fn error(&self, args: fmt::Arguments) -> Box<RuntimeError> {
        let data: Vec<_> = self
            .frames
            .iter()
            .rev()
            .map(|frame| {
                let line = *frame.function().chunk.get_line(frame.ip - 1).unwrap();
                let name = frame.function().name.clone();
                StackData::new(line, name)
            })
            .collect();

        RuntimeError::new(args, StackTrace(data))
    }
}

// These probably should be integration tests
#[cfg(test)]
mod tests {
    use std::str;

    use losk_core::Scanner;

    use crate::compiler::Compiler;
    use crate::vm::VM;

    fn test_program(src: &str, out: Option<&str>, err: Option<&str>) {
        println!("Testing source:\n{}", src);

        let mut scanner = Scanner::new();
        let compiler = Compiler::new();
        let fun = compiler.compile(scanner.scan_tokens(src)).unwrap();
        let mut output: Vec<u8> = Vec::new();

        let mut vm = VM::new(&mut output);
        let result = vm.run(fun);

        match (result, err) {
            (Err(out), Some(err)) => assert_eq!(err, out.to_string()),
            (Err(out), None) => {
                panic!("Not expecting any error, found '{}'", out)
            }
            (Ok(_), Some(err)) => panic!("Expecting an error '{}', found none.", err),
            _ => {}
        }

        if let Some(out) = out {
            assert_eq!(str::from_utf8(&output).unwrap(), out);
        }

        println!("\n\n");
    }

    #[test]
    fn test_programs() {
        let tests = [
            (
                include_str!("../../../data/print_expression.lox"),
                include_str!("../../../data/print_expression.lox.expected"),
            ),
            (
                include_str!("../../../data/var_assignment.lox"),
                include_str!("../../../data/var_assignment.lox.expected"),
            ),
            (
                include_str!("../../../data/block.lox"),
                include_str!("../../../data/block.lox.expected"),
            ),
            (
                include_str!("../../../data/if_else.lox"),
                include_str!("../../../data/if_else.lox.expected"),
            ),
            (
                include_str!("../../../data/while.lox"),
                include_str!("../../../data/while.lox.expected"),
            ),
            (
                include_str!("../../../data/for.lox"),
                include_str!("../../../data/for.lox.expected"),
            ),
            (
                include_str!("../../../data/print_function.lox"),
                include_str!("../../../data/print_function.lox.expected"),
            ),
            (
                include_str!("../../../data/fun_no_return.lox"),
                include_str!("../../../data/fun_no_return.lox.expected"),
            ),
            (
                include_str!("../../../data/fun_nil_return_print.lox"),
                include_str!("../../../data/fun_nil_return_print.lox.expected"),
            ),
            (
                include_str!("../../../data/fib.lox"),
                include_str!("../../../data/fib.lox.expected"),
            ),
            (
                include_str!("../../../data/capture_inner_variable.lox"),
                include_str!("../../../data/capture_inner_variable.lox.expected"),
            ),
            // (
            //     include_str!("../../../data/captured_closure.lox"),
            //     include_str!("../../../data/captured_closure.lox.expected"),
            // ),
        ];

        for (src, expected) in tests {
            test_program(src, Some(expected), None);
        }
    }

    #[test]
    fn test_stack_trace() {
        let src = include_str!("../../../data/function_stack_trace.lox");
        let exp = include_str!("../../../data/function_stack_trace.lox.expected");

        test_program(src, None, Some(exp));
    }
}
