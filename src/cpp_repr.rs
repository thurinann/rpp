use core::pin::Pin;

use crate::StackBox;

pub unsafe trait CppRepr: Sized {
    type Repr: Sized;

    #[inline(always)]
    fn cpp_ref(&self) -> &Self::Repr {
        unsafe { core::mem::transmute(self) }
    }

    #[inline(always)]
    fn cpp_mut(self: Pin<&mut Self>) -> Pin<&mut Self::Repr> {
        unsafe { core::mem::transmute(self) }
    }
}

#[doc(hidden)]
macro_rules! __impl_cpp_repr_for_primitive {
    ($t:ty) => {
        unsafe impl CppRepr for $t {
            type Repr = Self;
        }
    };
}

__impl_cpp_repr_for_primitive!(i8);
__impl_cpp_repr_for_primitive!(i16);
__impl_cpp_repr_for_primitive!(i32);
__impl_cpp_repr_for_primitive!(i64);
__impl_cpp_repr_for_primitive!(u8);
__impl_cpp_repr_for_primitive!(u16);
__impl_cpp_repr_for_primitive!(u32);
__impl_cpp_repr_for_primitive!(u64);
__impl_cpp_repr_for_primitive!(f32);
__impl_cpp_repr_for_primitive!(f64);
__impl_cpp_repr_for_primitive!(isize);
__impl_cpp_repr_for_primitive!(usize);
__impl_cpp_repr_for_primitive!(bool);
__impl_cpp_repr_for_primitive!(::core::ffi::c_void);

unsafe impl<'a, U: 'a, T> CppRepr for &'a T
where
    T: CppRepr<Repr = U>,
{
    type Repr = &'a U;
}

unsafe impl<U, T> CppRepr for *const T
where
    T: CppRepr<Repr = U>,
{
    type Repr = *const U;
}

unsafe impl<'a, U: 'a, T> CppRepr for &'a mut T
where
    T: CppRepr<Repr = U>,
{
    type Repr = &'a mut U;
}

unsafe impl<U, T> CppRepr for *mut T
where
    T: CppRepr<Repr = U>,
{
    type Repr = *mut U;
}

unsafe impl<'a, T> CppRepr for StackBox<'a, T>
where
    T: CppRepr,
{
    type Repr = StackBox<'a, <T as CppRepr>::Repr>;
}

unsafe impl<T> CppRepr for Pin<T>
where
    T: CppRepr,
{
    type Repr = Pin<<T as CppRepr>::Repr>;
}

unsafe impl<T> CppRepr for Option<T>
where
    T: CppRepr,
{
    type Repr = Option<<T as CppRepr>::Repr>;
}

unsafe impl CppRepr for () {
    type Repr = core::ffi::c_void;
}

#[doc(hidden)]
macro_rules! __impl_cpp_repr_for_fun {
    ($($arg:ident),+) => {
        unsafe impl<$($arg,)+ Ret> CppRepr for extern "C" fn($($arg,)+) -> Ret
        where
            $($arg: CppRepr,)+
            Ret: CppRepr,
        {
            type Repr = Self;
        }
    };
}

__impl_cpp_repr_for_fun!(T1);
__impl_cpp_repr_for_fun!(T1, T2);
__impl_cpp_repr_for_fun!(T1, T2, T3);
__impl_cpp_repr_for_fun!(T1, T2, T3, T4);
__impl_cpp_repr_for_fun!(T1, T2, T3, T4, T5);
__impl_cpp_repr_for_fun!(T1, T2, T3, T4, T5, T6);
__impl_cpp_repr_for_fun!(T1, T2, T3, T4, T5, T6, T7);
__impl_cpp_repr_for_fun!(T1, T2, T3, T4, T5, T6, T7, T8);
__impl_cpp_repr_for_fun!(T1, T2, T3, T4, T5, T6, T7, T8, T9);
__impl_cpp_repr_for_fun!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10);
__impl_cpp_repr_for_fun!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11);
__impl_cpp_repr_for_fun!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12);
__impl_cpp_repr_for_fun!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13);
__impl_cpp_repr_for_fun!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14);
__impl_cpp_repr_for_fun!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15);
__impl_cpp_repr_for_fun!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16);
