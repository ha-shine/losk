use std::ops::{Deref, DerefMut};

#[derive(Debug, Copy)]
pub(crate) struct UnsafeRef<T> {
    ptr: *const T,
}

impl<T> Clone for UnsafeRef<T> {
    fn clone(&self) -> Self {
        UnsafeRef { ptr: self.ptr }
    }
}

impl<T> UnsafeRef<T> {
    pub(crate) fn new(val: &T) -> Self {
        UnsafeRef {
            ptr: val as *const T,
        }
    }

    pub(crate) unsafe fn empty() -> Self {
        UnsafeRef {
            ptr: std::ptr::null()
        }
    }

    pub(crate) fn borrow_mut(&self) -> &mut T {
        unsafe {
            let mut_ptr = self.ptr as *mut T;
            &mut *mut_ptr as &mut T
        }
    }
}

impl<T> Deref for UnsafeRef<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.ptr as &T }
    }
}

impl<T> DerefMut for UnsafeRef<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            let mut_ptr = self.ptr as *mut T;
            &mut *mut_ptr as &mut T
        }
    }
}
