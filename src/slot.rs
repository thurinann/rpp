use core::mem::MaybeUninit;

use crate::CppRepr;

#[repr(transparent)]
pub struct Slot<T>(MaybeUninit<T>);

impl<T> Default for Slot<T> {
    fn default() -> Self {
        Self(MaybeUninit::uninit())
    }
}

impl<T> Slot<T> {
    #[inline(always)]
    pub fn as_mut_ptr(&mut self) -> *mut T {
        self.0.as_mut_ptr()
    }

    #[inline(always)]
    pub fn convert<U>(&mut self) -> &mut Slot<U>
    where
        T: CppRepr<Repr = U>,
    {
        unsafe { &mut *(self as *mut _ as *mut Slot<U>) }
    }
}
