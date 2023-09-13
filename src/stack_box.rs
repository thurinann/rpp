use core::marker::PhantomData;
use core::ops::{Deref, DerefMut};
use core::pin::Pin;
use core::ptr::drop_in_place;

use crate::{CppRepr, Slot};

#[repr(transparent)]
pub struct StackBox<'a, T: 'a> {
    data: *mut T,
    _marker: PhantomData<&'a T>,
}

impl<'a, T> StackBox<'a, T> {
    pub unsafe fn pin(data: &'a mut Slot<T>) -> Pin<Self> {
        Pin::new_unchecked(Self {
            data: data.as_mut_ptr(),
            _marker: PhantomData,
        })
    }
}

impl<'a, T> Deref for StackBox<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.data }
    }
}

impl<'a, T> DerefMut for StackBox<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.data }
    }
}

impl<T> Drop for StackBox<'_, T> {
    fn drop(&mut self) {
        unsafe {
            drop_in_place(self.data);
        }
    }
}
