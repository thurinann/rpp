use clang::Entity;
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use serde::{Deserialize, Serialize};

use crate::ty::Flags;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize, Deserialize)]
pub struct ClassMemberInfo {
    size: usize,
    align: usize,
}

impl ClassMemberInfo {
    pub fn parse(e: Entity<'_>) -> Option<Self> {
        let ty = e
            .get_child(Flags::TYPE_INDEX)?
            .get_type()?
            .get_canonical_type();
        Some(Self {
            size: ty.get_sizeof().ok()?,
            align: ty.get_alignof().ok()?,
        })
    }

    pub fn to_tokens(&self, id: u64, tokens: &mut TokenStream) {
        let ident = format_ident!("RPPClassMember{id}");
        let size = Literal::usize_unsuffixed(self.size);
        let align = Literal::usize_unsuffixed(self.align);
        quote!(
            #[repr(C, align(#align))]
            #[derive(Copy, Clone, Debug)]
            pub struct #ident([u8; #size]);

            unsafe impl rpp::CppRepr for #ident {
                type Repr = Self;
            }
        )
        .to_tokens(tokens);
    }
}
