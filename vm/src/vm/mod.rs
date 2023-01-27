use ahash::RandomState;
use intrusive_collections::LinkedList;
use std::collections::HashMap;
use std::fmt;
use std::io::Write;
use std::ptr::null_mut;

use native::clock;

use crate::chunk::*;
use crate::limits::{
    GC_HEAP_GROW_FACTOR, VM_MAX_CALLFRAME_COUNT, VM_MAX_STACK_SIZE, VM_STACK_TRACE_LIMIT,
};
use crate::object::*;
use crate::unsafe_ref::UnsafeRef;
use crate::value::ConstantValue;
use crate::vm::error::*;
use crate::vm::types::*;

mod error;
pub mod native;
pub mod object;
mod types;

pub struct VM<'a> {
    // The stack as a growable vector. In textbook, this is represented by an array and a stack
    // pointer that always point to one element past the top. This is not required here since the
    // vector already give us those functionality.
    stack: [StackValue; VM_MAX_STACK_SIZE],
    s_ptr: *mut StackValue,

    // List of objects currently allocated on the heap
    // Note that the heap objects need to be mutable because of two reasons -
    //   - Garbage collector will mark the objects
    //   - Closure's captured values will be updated with `SetUpvalue` instruction
    //
    // Regarding clippy's vec box issue, if I allocate an object and push it to the vector, the
    // pointer address will change since it's now moved to the heap. I need pointer to be stable
    // even after pushing, so Boxing is still necessary.
    #[allow(clippy::vec_box)]
    objects: LinkedList<HeapAdapter>,

    // The call frame is limited and is allocated on the rust stack for faster access compared to
    // a heap. This comes with limitation that the frame count will be limited and pre-allocation
    // when the VM starts.
    frames: [CallFrame; VM_MAX_CALLFRAME_COUNT],
    frame_count: usize,
    c_frame: UnsafeRef<CallFrame>,

    // List of upvalues that have not been closed. They are objects here because there's not a good
    // way to point directly to the UpvalueObject unless I go the route of double indirection
    // by wrapping the UpvalueObject itself in an Rc.
    open_upvalues: Vec<UnsafeRef<Object>>,

    // Globals are used to map between a variable defined on the global scope to it's corresponding
    // value. The values are of type `StackValue` because the globals can be mapped to either stack
    // or heap values. The expression after the var declaration statement is pushed onto the stack
    // so it makes sense to map to a `StackValue`. Care should be taken so that the GC scans this
    // table for reachability too.
    globals: HashMap<&'static str, StackValue, RandomState>,

    // The VM will write the output strings into this stdout
    stdout: &'a mut dyn Write,

    // GC related fields
    bytes_allocated: usize,
    next_gc: usize,
}

// Be aware the VM will leak the function passed into to create a static reference to it and
// it will not be reclaimed
impl<'a> VM<'a> {
    pub fn new(stdout: &'a mut dyn Write) -> Self {
        let mut vm = VM {
            stack: [Default::default(); VM_MAX_STACK_SIZE],
            s_ptr: null_mut(),

            objects: LinkedList::new(HeapAdapter::new()),
            frames: [Default::default(); VM_MAX_CALLFRAME_COUNT],
            frame_count: 0,
            c_frame: Default::default(),
            open_upvalues: Vec::new(),
            globals: HashMap::default(),
            stdout,
            bytes_allocated: 0,
            next_gc: 1024 * 1024,
        };

        vm.s_ptr = vm.stack.as_mut_ptr();
        vm.allocate_and_push(HeapValue::NativeFunction(NativeFunction::new(
            "clock", 0, clock,
        )));

        let clock = vm.pop();
        vm.globals.insert("clock", clock);
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
            Upvalue(UnsafeRef<Object>),
            Pointer(*mut StackValue),
        }

        // A bit disappointing that I need a temporary vector just for this. If this proves to be
        // too much of a hit on performance, I can always copy the code inline.
        let temp: Vec<_> = (0..fun.upvalue_count)
            .map(|idx| fun.upvalues[idx])
            .map(|upvalue| {
                if upvalue.is_local {
                    unsafe { UpvalueOrStackPosition::Pointer(slots.add(upvalue.index)) }
                } else {
                    UpvalueOrStackPosition::Upvalue(p_closure.captured[upvalue.index])
                }
            })
            .collect();

        for value in temp {
            match value {
                UpvalueOrStackPosition::Upvalue(val) => captured.push(val),

                // Open upvalues are pushed onto open upvalue list in the `capture_upvalue`,
                // so the temporary values here are safe from GC even though the new call frame has
                // not been pushed onto stack yet, and the upvalues here are not reachable from callframe.
                UpvalueOrStackPosition::Pointer(ptr) => captured.push(self.capture_upvalue(ptr)),
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
        let ip = main_closure.fun.chunk.instructions.as_ptr() as *mut Instruction;
        self.allocate_and_push(HeapValue::Closure(main_closure));

        let fun = if let StackValue::Obj(obj) = self.peek_stack(0) {
            *obj
        } else {
            panic!("Unreachable");
        };

        // The slots should point to the closure
        self.execute_callframe(CallFrame {
            fun,
            ip,
            slots: unsafe { self.s_ptr.offset(-1) },
        })?;
        self.interpret()
    }

    fn interpret(&mut self) -> VmResult<()> {
        loop {
            // Instruction is cheap to copy, though the run loop is very sensitive to performance
            // and not sure this would affect the runtime severely.
            let instruction = unsafe {
                let ip = self.current_frame().ip;
                let instruction = *ip as Instruction;
                self.current_frame_mut().ip = ip.add(1);
                instruction
            };

            match instruction {
                Instruction::Constant(val) => self.execute_constant(val)?,
                Instruction::LiteralTrue => self.push(StackValue::Bool(true)),
                Instruction::LiteralFalse => self.push(StackValue::Bool(false)),
                Instruction::LiteralNil => self.push(StackValue::Nil),
                Instruction::Pop => {
                    self.pop();
                }

                Instruction::GetGlobal(constant) => {
                    let name = self.get_constant(constant);

                    match self.globals.get(name) {
                        Some(sval) => self.push(*sval),
                        None => {
                            return Err(self.error(format_args!("Undefined variable '{}'.", name)))
                        }
                    }
                }

                Instruction::DefineGlobal(constant) => {
                    // The global name is stored as a constant value in the constant pool, read
                    // from the pool to get the name.
                    let name = self.get_constant(constant);

                    let val = self.pop();
                    self.globals.insert(name, val);
                }

                Instruction::SetGlobal(val) => self.execute_set_global(val)?,

                Instruction::GetLocal(pos) => unsafe {
                    let ptr = self.c_frame.slots.add(pos);
                    self.push(*ptr);
                },

                Instruction::SetLocal(pos) => unsafe {
                    let ptr = self.c_frame.slots.add(pos);
                    *ptr = *self.peek_stack(0);
                },

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
                    let top = self.peek_stack(0);
                    if let HeapValue::Closure(closure) = &self.current_frame().fun.value {
                        self.set_upvalue(closure.captured[index], *top);
                    } else {
                        panic!("Unreachable")
                    }
                }

                Instruction::GetProperty(constant) => {
                    let instance_slot = self.pop();

                    let obj = match instance_slot {
                        StackValue::Obj(ref obj) => obj,
                        _ => return Err(self.error(format_args!("Only instances have properties"))),
                    };

                    let instance = match &obj.value {
                        HeapValue::Instance(instance) => instance,
                        HeapValue::BoundMethod(method) => method.instance(),
                        _ => return Err(self.error(format_args!("Only instances have properties"))),
                    };

                    let field = self.get_constant(constant);

                    match instance.fields.get(field) {
                        Some(val) => {
                            self.push(*val);
                        }
                        None => {
                            let bound_method =
                                self.bind_method(instance_slot, instance.class(), field)?;
                            self.allocate_and_push(HeapValue::BoundMethod(bound_method));
                        }
                    };
                }

                Instruction::SetProperty(constant) => {
                    let value_slot = self.pop();
                    let instance_slot = self.pop();

                    let obj = match instance_slot {
                        StackValue::Obj(obj) => obj,
                        _ => return Err(self.error(format_args!("Only instances have properties"))),
                    };

                    if let HeapValue::Instance(instance) = &mut obj.borrow_mut().value {
                        let field = self.get_constant(constant);

                        instance.fields.insert(field, value_slot);
                        self.push(value_slot);
                    } else {
                        return Err(self.error(format_args!("Only instances have properties")));
                    }
                }

                Instruction::GetSuper(constant) => {
                    let name = self.get_constant(constant);

                    // On stack -> [this, super]
                    let superclass_slot = self.pop();
                    let superclass_obj = match superclass_slot {
                        StackValue::Obj(obj) => obj,
                        _ => panic!("Unreachable"),
                    };
                    let superclass = match &superclass_obj.value {
                        HeapValue::Class(class) => class,
                        _ => panic!("Unreachable"),
                    };

                    let instance = self.pop();
                    let bound_method = self.bind_method(instance, superclass, name)?;
                    self.allocate_and_push(HeapValue::BoundMethod(bound_method));
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
                    match self.is_falsey(*self.peek_stack(0)) {
                        Ok(StackOrHeap::Stack(StackValue::Bool(val))) => {
                            if val {
                                self.current_frame_mut().jump(dist - 1)
                            }
                        }
                        Err(msg) => return Err(self.error(format_args!("{}", msg))),
                        _ => panic!("Unreachable"),
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

                // Probably should be separated into it's own function
                Instruction::Call(ArgCount(count)) => {
                    if self.frame_count == VM_MAX_CALLFRAME_COUNT {
                        return Err(self.error(format_args!("Stack overflow.")));
                    }

                    // The previous instructions must have pushed `count` amount to stack, so
                    // the function value would exist at stack[-count].
                    let obj = self.peek_stack(count as isize);
                    self.execute_call(*obj, count)?;
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
                    let val = self.current_frame().function().chunk.get_constant(idx);

                    if let Some(ConstantValue::Fun(fun)) = val {
                        let closure = self.make_closure(fun);
                        self.allocate_and_push(HeapValue::Closure(closure));
                    } else {
                        panic!("Unreachable")
                    }
                }

                Instruction::CloseUpvalue => unsafe {
                    self.close_upvalues(self.s_ptr.offset(-1));
                    self.pop();
                },

                Instruction::Return => {
                    // Ensure the current frame's closure is dropped after returning.
                    let frame = self.frames[self.frame_count - 1];
                    self.frame_count -= 1;

                    // The return value of a function should be on top of the stack, so it'll be
                    // pushed onto the top of the stack.
                    self.close_upvalues(frame.slots);
                    let result = self.pop();

                    if self.frame_count == 0 {
                        self.pop();
                        return Ok(());
                    }

                    // After calling a function, the stack top needs to point to the place where
                    // the function information is stored previously (so frame.slots).
                    // To get back there, I need to pop stack.len() - slots.
                    self.s_ptr = frame.slots;
                    self.push(result);
                    self.c_frame = UnsafeRef::new(&self.frames[self.frame_count - 1]);
                }

                Instruction::Class(Constant(idx)) => {
                    let name = match self.current_frame().function().chunk.get_constant(idx) {
                        Some(ConstantValue::Str(name)) => name.as_str(),
                        _ => panic!("Unreachable"),
                    };

                    let cls = Class::new(name);
                    self.allocate_and_push(HeapValue::Class(cls));
                }

                Instruction::Method(Constant(idx)) => {
                    let body = self.pop();
                    let name = match self.current_frame().function().chunk.get_constant(idx) {
                        Some(ConstantValue::Str(name)) => name,
                        _ => panic!("Unreachable"),
                    };

                    let class_slot = self.peek_stack(0);
                    let obj = match class_slot {
                        StackValue::Obj(obj) => obj,
                        _ => panic!("Unreachable, expecting class below closure"),
                    };

                    let class = match &mut obj.borrow_mut().value {
                        HeapValue::Class(class) => class,
                        _ => panic!("Unreachable, expecting class below closure"),
                    };

                    class.methods.insert(name, body);
                }

                // When invoke is called, the args must have been pushed onto the stack because of
                // the `argument_list` call in compiler. Plus, the instance must have been pushed
                // on the stack just after the `argument_list` because an expression comes
                // before the `.` syntax. That is the receiver. In that case, I don't need to
                // create a separate bound method but instead invoke the method from class directly.
                Instruction::Invoke(Invoke { name, args }) => {
                    let name = match self.current_frame().function().chunk.get_constant(name.0) {
                        Some(ConstantValue::Str(name)) => name,
                        _ => panic!("Unreachable"),
                    };

                    let args = args.0;
                    let receiver = *self.peek_stack(args as isize);
                    let instance_obj = match receiver {
                        StackValue::Obj(obj) => obj,
                        _ => return Err(self.error(format_args!("Only instances have methods."))),
                    };
                    let instance = match &instance_obj.value {
                        HeapValue::Instance(instance) => instance,
                        _ => return Err(self.error(format_args!("Only instances have methods."))),
                    };

                    self.invoke_from_class(instance.class(), instance, name, args)?;
                }

                Instruction::SuperInvoke(Invoke { name, args }) => {
                    let name = match self.current_frame().function().chunk.get_constant(name.0) {
                        Some(ConstantValue::Str(name)) => name,
                        _ => panic!("Unreachable"),
                    };

                    let superclass_slot = self.pop();
                    let superclass_obj = match superclass_slot {
                        StackValue::Obj(obj) => obj,
                        _ => panic!("Unreachable"),
                    };
                    let superclass = match &superclass_obj.value {
                        HeapValue::Class(class) => class,
                        _ => panic!("Unreachable"),
                    };

                    let args = args.0;
                    let receiver = *self.peek_stack(args as isize);
                    let instance_obj = match receiver {
                        StackValue::Obj(obj) => obj,
                        _ => return Err(self.error(format_args!("Only instances have methods."))),
                    };
                    let instance = match &instance_obj.value {
                        HeapValue::Instance(instance) => instance,
                        _ => return Err(self.error(format_args!("Only instances have methods."))),
                    };

                    self.invoke_from_class(superclass, instance, name, args)?;
                }

                Instruction::Inherit => {
                    let sup = self.peek_stack(1);
                    let sub = self.peek_stack(0);
                    let (sup_obj, sub_obj) = match (sup, sub) {
                        (StackValue::Obj(sup_obj), StackValue::Obj(sub_obj)) => (sup_obj, sub_obj),
                        _ => return Err(self.error(format_args!("Superclass must be a class"))),
                    };

                    let (sup_cls, sub_cls) = match (&sup_obj.value, &mut sub_obj.borrow_mut().value)
                    {
                        (HeapValue::Class(sup_cls), HeapValue::Class(sub_cls)) => {
                            (sup_cls, sub_cls)
                        }
                        _ => return Err(self.error(format_args!("Superclass must be a class"))),
                    };

                    sub_cls
                        .methods
                        .extend(sup_cls.methods.iter().map(|(k, v)| (*k, *v)));

                    // Pop the subclass
                    self.pop();
                }
            }
        }
    }

    fn get_constant(&mut self, constant: Constant) -> &'static str {
        match self
            .current_frame()
            .function()
            .chunk
            .get_constant(constant.0)
        {
            Some(ConstantValue::Str(name)) => name,
            _ => panic!("Unreachable"),
        }
    }

    fn execute_callframe(&mut self, cf: CallFrame) -> VmResult<()> {
        self.frames[self.frame_count] = cf;
        self.c_frame = UnsafeRef::new(&self.frames[self.frame_count]);
        self.frame_count += 1;
        Ok(())
    }

    fn execute_set_global(&mut self, constant: Constant) -> VmResult<()> {
        // Note that the set variable doesn't pop the value from the stack because the set assignment
        // is an expression statement and there always is a pop instruction after expression statement.
        let val = *self.peek_stack(0);
        let name = self.get_constant(constant);

        match self.globals.get_mut(name) {
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
            .get_constant(con.0)
            .ok_or_else(|| self.error(format_args!("Unknown constant")))?;

        match constant {
            ConstantValue::Double(val) => self.push(StackValue::Num(*val)),
            ConstantValue::Bool(val) => self.push(StackValue::Bool(*val)),
            ConstantValue::Str(val) => self.push(StackValue::Str(val)),
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

    fn execute_call(&mut self, ptr: StackValue, args: usize) -> VmResult<()> {
        if let StackValue::Obj(obj) = ptr {
            match &obj.value {
                HeapValue::Closure(_) => self.call_closure(obj, args),

                HeapValue::NativeFunction(fun) => {
                    if args != fun.arity {
                        return Err(self.error(format_args!(
                            "Expected {} arguments but got {}.",
                            fun.arity, args
                        )));
                    }

                    let result = unsafe {
                        let ptr_beg = self.s_ptr.offset(-(args as isize));
                        let arguments = std::slice::from_raw_parts(ptr_beg, args);
                        (fun.fun)(arguments)
                    };

                    self.pop_n(args as isize + 1); // args + function data
                    if let Some(result) = result {
                        self.store_native_value(result)?;
                    }

                    Ok(())
                }

                HeapValue::Class(cls) => {
                    let instance = Instance::new(UnsafeRef::clone(&obj));
                    let initializer = cls.methods.get("init").cloned();
                    self.allocate_and_push(HeapValue::Instance(instance));

                    // The function is at -count position, replace that with the instance
                    // that is just created
                    let instance_slot = self.pop();
                    *self.peek_stack_mut(args as isize) = instance_slot;

                    // If there's an initializer, call the initializer. If there's
                    // not but the arg count is not 0, the user must have passed
                    // arguments to the default initializer.
                    if let Some(initializer) = initializer {
                        self.execute_call(initializer, args)
                    } else if args != 0 {
                        Err(self.error(format_args!("Expected 0 arguments but got {}.", args)))
                    } else {
                        Ok(())
                    }
                }

                HeapValue::BoundMethod(method) => {
                    // I need to push the receiver as the zero-slot local, instead of the
                    // bound method itself. `this` is basically the first argument for
                    // the method.
                    *self.peek_stack_mut(args as isize) = method.receiver;
                    self.call_closure(method.method, args)
                }

                HeapValue::Str(_) | HeapValue::Upvalue(_) | HeapValue::Instance(_) => {
                    Err(self.error(format_args!("Can only call functions and classes")))
                }
            }
        } else {
            Err(self.error(format_args!("Can only call functions and classes")))
        }
    }

    fn call_closure(&mut self, closure: UnsafeRef<Object>, args: usize) -> VmResult<()> {
        let ip = if let HeapValue::Closure(closure) = &closure.value {
            if closure.fun.arity != args {
                return Err(self.error(format_args!(
                    "Expected {} arguments but got {}.",
                    closure.fun.arity, args
                )));
            }

            closure.fun.chunk.instructions.as_ptr() as *mut Instruction
        } else {
            return Err(self.error(format_args!("Can only call functions and classes")));
        };

        self.execute_callframe(CallFrame {
            fun: closure,
            ip,
            slots: unsafe { self.s_ptr.offset(-(args as isize) - 1) },
        })
    }

    fn invoke_from_class(
        &mut self,
        class: &Class,
        instance: &Instance,
        name: &str,
        args: usize,
    ) -> VmResult<()> {
        // Wait, instance is no longer reachable from stack here. Because the instance's field is
        // put onto the place where the instance should be.
        if let Some(field) = instance.fields.get(name) {
            *self.peek_stack_mut(args as isize) = *field;
            self.execute_call(*field, args)?;
            return Ok(());
        }

        match class.methods.get(name).cloned() {
            Some(method) => self.execute_call(method, args),
            None => Err(self.error(format_args!("Undefined property {}.", name))),
        }
    }

    fn store_op_result(&mut self, res: OpResult) -> VmResult<()> {
        match res {
            Ok(StackOrHeap::Stack(val)) => {
                self.push(val);
                Ok(())
            }
            Ok(StackOrHeap::Heap(val)) => {
                self.allocate_and_push(val);
                Ok(())
            }
            Err(msg) => Err(self.error(format_args!("{}", msg))),
        }
    }

    fn is_falsey(&self, val: StackValue) -> OpResult {
        match val {
            StackValue::Bool(val) => Ok(StackOrHeap::Stack(StackValue::Bool(!val))),
            StackValue::Nil => Ok(StackOrHeap::Stack(StackValue::Bool(true))),
            _ => Ok(StackOrHeap::Stack(StackValue::Bool(false))),
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
        if let (StackValue::Num(lhs), StackValue::Num(rhs)) = (&lhs, &rhs) {
            Ok(StackOrHeap::Stack(StackValue::Num(lhs + rhs)))
        } else if let (Some(lhs), Some(rhs)) = (lhs.as_string(), rhs.as_string()) {
            let mut res = String::with_capacity(lhs.len() + rhs.len());
            res += lhs;
            res += rhs;
            Ok(StackOrHeap::Heap(HeapValue::Str(res)))
        } else {
            Err("Expect both operands to be either numbers or strings.")
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
        self.is_falsey(val)
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
            ConstantValue::Str(val) => self.allocate_and_push(HeapValue::Str(val)),
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
            StackValue::Str(val) => {
                writeln!(self.stdout, "{}", val).unwrap();
            }
            StackValue::Obj(object) => {
                writeln!(self.stdout, "{}", object.value).unwrap();
            }
            StackValue::Nil => {
                writeln!(self.stdout, "nil").unwrap();
            }
        };
    }

    fn capture_upvalue(&mut self, ptr: *mut StackValue) -> UnsafeRef<Object> {
        for open in &self.open_upvalues {
            match &open.value {
                HeapValue::Upvalue(UpvalueState::Open(pos)) => {
                    if *pos == ptr {
                        return *open;
                    }
                }
                _ => panic!("Found non-upvalue in open upvalue list"),
            }
        }

        self.allocate_and_push(HeapValue::Upvalue(UpvalueState::Open(ptr)));
        if let StackValue::Obj(obj) = self.pop() {
            self.open_upvalues.push(obj);
            obj
        } else {
            panic!("Unreachable")
        }
    }

    // Close all the upvalues that are above the stack index given in the argument.
    // The open upvalues are ordered by their stack position from the highest to lowest. The loop
    // will stop as soon as the cursor reaches an open value whose stack position is lower than the
    // given index.
    fn close_upvalues(&mut self, top: *mut StackValue) {
        let upvalues = std::mem::take(&mut self.open_upvalues);
        self.open_upvalues = upvalues
            .into_iter()
            .filter(|val| match &val.value {
                HeapValue::Upvalue(UpvalueState::Open(pos)) => {
                    if *pos >= top {
                        unsafe {
                            val.borrow_mut().value =
                                HeapValue::Upvalue(UpvalueState::Closed(**pos as StackValue));
                        }
                        false
                    } else {
                        true
                    }
                }
                _ => panic!("Found non-upvalue in open upvalue list"),
            })
            .collect();
    }

    // This will return the stack object the given upvalue object is pointing to, which means
    // the original stack value if it's an open upvalue, or the closed value itself.
    // The method assumes the given object is an upvalue, and panic if not.
    fn get_upvalue(&self, obj: &UnsafeRef<Object>) -> StackValue {
        match &obj.value {
            HeapValue::Upvalue(val) => match val {
                UpvalueState::Open(pos) => unsafe { **pos as StackValue },
                UpvalueState::Closed(val) => *val,
            },
            _ => panic!("Object is not an upvalue"),
        }
    }

    fn set_upvalue(&mut self, obj: UnsafeRef<Object>, value: StackValue) {
        match &obj.value {
            HeapValue::Upvalue(val) => {
                if let UpvalueState::Open(pos) = val {
                    unsafe {
                        **pos = value;
                    }
                } else {
                    obj.borrow_mut().value = HeapValue::Upvalue(UpvalueState::Closed(value));
                }
            }
            _ => panic!("Object is not an upvalue"),
        };
    }

    fn bind_method(
        &self,
        instance: StackValue,
        class: &Class,
        name: &str,
    ) -> VmResult<BoundMethod> {
        let method = match class.methods.get(name) {
            Some(method) => *method,
            None => return Err(self.error(format_args!("Undefined property '{}'.", name))),
        };

        if let StackValue::Obj(obj) = method {
            Ok(BoundMethod::new(instance, obj))
        } else {
            panic!("Unreachable")
        }
    }

    fn peek_stack(&self, dist: isize) -> &StackValue {
        unsafe { &*self.s_ptr.offset(-dist - 1) as &StackValue }
    }

    fn peek_stack_mut(&mut self, dist: isize) -> &mut StackValue {
        unsafe { &mut *self.s_ptr.offset(-dist - 1) as &mut StackValue }
    }

    fn push(&mut self, val: StackValue) {
        unsafe {
            *self.s_ptr = val;
            self.s_ptr = self.s_ptr.offset(1);
        }
    }

    fn pop(&mut self) -> StackValue {
        unsafe {
            self.s_ptr = self.s_ptr.offset(-1);
            *self.s_ptr as StackValue
        }
    }

    fn pop_n(&mut self, n: isize) {
        unsafe {
            self.s_ptr = self.s_ptr.offset(-n);
        }
    }

    fn allocate_and_push(&mut self, val: HeapValue) {
        let sval = self.allocate_object(val);
        self.push(sval);
    }

    // This is where the GC will make the decision on doing GC which means be careful using this
    // method (i.e ensure the object being passed to here is already reachable from stack).
    // TODO: Stress GC if feature is enabled (and also logging GC)
    fn allocate_object(&mut self, val: HeapValue) -> StackValue {
        self.bytes_allocated += std::mem::size_of_val(&val);
        self.objects.push_front(Object::new(val));

        // Temporarily push the object on to stack so that it's reachable in the sweeping phase
        self.push(StackValue::Obj(UnsafeRef::new(
            self.objects.front().get().unwrap(),
        )));

        // Check if Gc should start mark-sweep
        if self.bytes_allocated > self.next_gc {
            self.mark();
            self.sweep();

            self.next_gc = self.bytes_allocated * GC_HEAP_GROW_FACTOR;
        }

        self.pop()
    }

    // In the text book, the marking phase is separated into two parts -
    // - Marking the roots where those reachable from the stack, global table, open upvalues,
    //   and call-frames are marked. During this, if any references are found, they are pushed onto
    //   gray list which are processed again in the second phase.
    // - Tracing reference phase where the objects from gray list are walked recursively and mark
    //   every objects it found.
    // I've combined both phase into one mark phase by following the references along. Essentially,
    // I'm using DFS while the textbook uses BFS and it shouldn't really affect the runtime.
    fn mark(&mut self) {
        // Mark the objects from the stack recursively
        let mut s_ptr = self.stack.as_mut_ptr();
        while s_ptr != self.s_ptr {
            unsafe {
                if let StackValue::Obj(obj) = *s_ptr {
                    Self::mark_object(obj.borrow_mut());
                }
                s_ptr = s_ptr.add(1);
            }
        }

        // Mark the global table
        for val in self.globals.values() {
            if let StackValue::Obj(obj) = val {
                Self::mark_object(obj.borrow_mut());
            }
        }

        // Walk through the call frames
        for frame in (0..self.frame_count).map(|val| self.frames[val]) {
            Self::mark_object(frame.fun.borrow_mut());
        }

        // Also walk through the open upvalue list
        for val in &self.open_upvalues {
            Self::mark_object(val.borrow_mut());
        }
    }

    fn mark_object(obj: &mut Object) {
        if obj.marked.get() {
            return;
        }

        // Mark the given object itself
        obj.marked.replace(true);

        match &obj.value {
            // Mark all the captured values if it's a closure
            HeapValue::Closure(closure) => {
                for captured in &closure.captured {
                    Self::mark_object(captured.borrow_mut());
                }
            }

            // If it's a closed upvalue and that closed value is pointing to another object,
            // mark them recursively too. I don't have to mark open value because they would have
            // been marked while walking through the stack
            HeapValue::Upvalue(val) => {
                if let UpvalueState::Closed(StackValue::Obj(upvalue)) = val {
                    Self::mark_object(upvalue.borrow_mut());
                }
            }

            HeapValue::Instance(ins) => {
                Self::mark_object(ins.class.borrow_mut());

                for field in ins.fields.values() {
                    // Fields can contain other stack values that don't require heap allocations
                    if let StackValue::Obj(obj) = field {
                        Self::mark_object(obj.borrow_mut())
                    }
                }
            }

            HeapValue::BoundMethod(method) => {
                Self::mark_object(method.method.borrow_mut());
                match &method.receiver {
                    StackValue::Obj(obj) => Self::mark_object(obj.borrow_mut()),
                    _ => panic!("Unreachable"),
                }
            }

            HeapValue::Class(cls) => {
                for method in cls.methods.values() {
                    match method {
                        StackValue::Obj(obj) => Self::mark_object(obj.borrow_mut()),
                        _ => panic!("Found StackValue in method"),
                    }
                }
            }

            // These don't need recursion, they contains no fields
            HeapValue::Str(_) | HeapValue::NativeFunction(_) => {}
        }
    }

    fn sweep(&mut self) {
        let mut cursor = self.objects.cursor_mut();
        cursor.move_next();

        while let Some(val) = cursor.get() {
            if val.marked.get() {
                val.marked.replace(false);
                cursor.move_next();
            } else {
                cursor.remove();
            }
        }
    }

    fn current_frame(&self) -> &CallFrame {
        &self.c_frame
    }

    fn current_frame_mut(&mut self) -> &mut CallFrame {
        &mut self.c_frame
    }

    fn error(&self, args: fmt::Arguments) -> Box<RuntimeError> {
        let data: Vec<_> = (0..self.frame_count)
            .rev()
            .map(|val| self.frames[val])
            .take(VM_STACK_TRACE_LIMIT)
            .map(|frame| unsafe {
                let line = *frame
                    .function()
                    .chunk
                    .get_line(
                        frame
                            .ip
                            .offset_from(frame.function().chunk.instructions.as_ptr())
                            as usize,
                    )
                    .unwrap();
                let name = frame.function().name.clone();
                StackData::new(line, name)
            })
            .collect();

        RuntimeError::new(args, StackTrace(data))
    }
}
