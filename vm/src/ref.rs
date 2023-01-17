use std::fmt::{Debug, Formatter};
use std::ops::{Deref, DerefMut};
use std::ptr::null;

// UnsafeRef is a wrapper around the raw pointer that provides the same ergonomics as RefCell, but
// doesn't do any runtime checks. The referred-to object is expected to be alive for the lifetime
// of this reference. This is very un-rust but necessary for GC implementation, and be very
// careful with its usage.
pub(crate) struct UnsafeRef<T> {
    ptr: *const T,
}

impl<T> PartialEq for UnsafeRef<T> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

impl<T> Debug for UnsafeRef<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:p}", self.ptr)
    }
}

impl<T> Clone for UnsafeRef<T> {
    fn clone(&self) -> Self {
        UnsafeRef { ptr: self.ptr }
    }
}

impl<T> Copy for UnsafeRef<T> {}

impl<T> UnsafeRef<T> {
    pub(crate) fn new(obj: &T) -> Self {
        UnsafeRef { ptr: obj }
    }

    // This is to initialise the VM with empty call frames, although it's a bit dangerous to
    // expose this crate wide.
    pub(crate) fn empty() -> Self {
        UnsafeRef { ptr: null() }
    }

    pub(crate) fn borrow(&self) -> &T {
        unsafe { &*self.ptr as &T }
    }

    pub(crate) fn borrow_mut(&mut self) -> &mut T {
        unsafe {
            let mut_ptr = self.ptr as *mut T;
            &mut *mut_ptr as &mut T
        }
    }
}

impl<T> Deref for UnsafeRef<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.borrow()
    }
}

impl<T> DerefMut for UnsafeRef<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.borrow_mut()
    }
}
