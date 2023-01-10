use std::borrow::{Borrow, BorrowMut};

// UnsafeRef is a wrapper around the raw pointer that provides the same ergonomics as RefCell, but
// doesn't do any runtime checks. The referred-to object is expected to be alive for the lifetime
// of this reference. This is very un-rust but necessary for GC implementation, and be very
// careful with its usage.
#[derive(PartialEq)]
pub(crate) struct UnsafeRef<T> {
    ptr: *const T,
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
}

impl<T> Borrow<T> for UnsafeRef<T> {
    fn borrow(&self) -> &T {
        unsafe { &*self.ptr as &T }
    }
}

impl<T> BorrowMut<T> for UnsafeRef<T> {
    fn borrow_mut(&mut self) -> &mut T {
        unsafe {
            let mut_ptr = self.ptr as *mut T;
            &mut *mut_ptr as &mut T
        }
    }
}

// UnsafeWeak is a similar wrapper like UnsafeRef, but check the referent before borrowing. Although
// these methods could be implemented on the UnsafeRef, I would like the two types to be visually
// difference enough to be noticeable on a glance.
#[derive(Copy, Clone)]
pub(crate) struct UnsafeWeak<T> {
    ptr: *const T,
}

impl<T> UnsafeWeak<T> {
    pub(crate) fn new(obj: &T) -> Self {
        UnsafeWeak { ptr: obj }
    }

    pub(crate) fn try_borrow(&self) -> Option<&T> {
        if self.ptr.is_null() {
            None
        } else {
            unsafe { Some(&*self.ptr as &T) }
        }
    }

    fn try_borrow_mut(&mut self) -> Option<&mut T> {
        if self.ptr.is_null() {
            None
        } else {
            unsafe {
                let mut_ptr = self.ptr as *mut T;
                Some(&mut *mut_ptr as &mut T)
            }
        }
    }
}
