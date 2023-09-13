use core::pin::Pin;

use crate::{Slot, StackBox};

#[doc(hidden)]
pub unsafe trait Move {
    unsafe fn move_to_ptr(value: Pin<StackBox<Self>>, dst: *mut core::ffi::c_void)
    where
        Self: Sized;

    fn move_to_slot<'a>(
        value: Pin<StackBox<'a, Self>>,
        slot: &'a mut Slot<Self>,
    ) -> Pin<StackBox<'a, Self>>
    where
        Self: Sized,
    {
        unsafe {
            Self::move_to_ptr(value, slot.as_mut_ptr() as _);
            StackBox::pin(slot)
        }
    }
}
