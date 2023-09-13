#![allow(unused_imports)]
mod cpp_repr;
mod move_to;
mod slot;
mod stack_box;

pub(crate) use crate as rpp;
pub use cpp_repr::CppRepr;
pub use move_to::Move;
pub use rpp_impl::{
    class, enum_val, ex, expand, export, init, par_ty, ty, val, var, wrap_const, wrap_enum,
    wrap_flags, wrap_struct, wrap_template,
};
pub use slot::Slot;
pub use stack_box::StackBox;

rpp_impl::__init_metadata!();

pub struct SParam<const N: isize>;
pub struct UParam<const N: usize>;
pub struct BParam<const N: bool>;
pub struct CParam<const N: char>;

#[cfg(debug_assertions)]
#[doc(hidden)]
#[inline(always)]
pub fn test_repr<U, T>()
where
    T: CppRepr<Repr = U>,
{
}

#[macro_export]
#[doc(hidden)]
macro_rules! const_assert_eq {
    ($left:expr, $right:expr $(,)?) => {
        const _: [(); $left] = [(); $right];
    };
}
