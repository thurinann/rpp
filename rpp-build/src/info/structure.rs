use clang::{Entity, EvaluationResult as ER};
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use serde::{Deserialize, Serialize};
use syn::spanned::Spanned;
use syn::{Ident, LitStr};

use crate::ty::Flags;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct StructInfo {
    name: String,
    flags: Flags,
    size: usize,
    align: usize,
}

impl StructInfo {
    pub fn parse(e: Entity) -> Option<Self> {
        let ty = e
            .get_child(Flags::TYPE_INDEX)?
            .get_typedef_underlying_type()?
            .get_canonical_type();
        Some(Self {
            name: ty.get_display_name(),
            flags: match e.get_child(Flags::FLAGS_INDEX)?.evaluate() {
                Some(ER::UnsignedInteger(v)) => Flags::from_bits_truncate(v as _),
                _ => return None,
            },
            size: ty.get_sizeof().ok()?,
            align: ty.get_alignof().ok()?,
        })
    }

    pub fn to_tokens(&self, id: u64, tokens: &mut TokenStream) {
        let ident = format_ident!("RPPStruct{id}");
        let size = Literal::usize_unsuffixed(self.size);
        let align = Literal::usize_unsuffixed(self.align);
        let (internal, repr) = if self.is_trivially_relocatable() {
            (
                quote!([u8; #size], ::core::marker::PhantomData<*const u8>),
                quote!(C, align(#align)),
            )
        } else {
            (
                quote!(
                [u8; #size],
                ::core::marker::PhantomData<*const u8>,
                ::core::marker::PhantomPinned
                ),
                quote!(C, align(#align)),
            )
        };
        quote!(
            #[repr(#repr)]
            #[derive(Debug)]
            pub struct #ident(#internal);
            unsafe impl rpp::CppRepr for #ident {
                type Repr = Self;
            }
        )
        .to_tokens(tokens);
        if self.is_trivially_copyable() {
            self.copy_impl(&ident, tokens);
        } else if self.is_copy_constructible() && self.is_trivially_relocatable() {
            self.clone_impl(&ident, id, tokens);
        } else if self.is_move_constructible() && !self.is_trivially_relocatable() {
            self.move_impl(&ident, id, tokens);
        }
        if self.is_destructible() && !self.is_trivially_destructible() {
            self.drop_impl(&ident, id, tokens);
        }
    }

    pub fn wrappers(&self, id: u64) -> Vec<String> {
        let mut result = Vec::new();
        if self.is_copy_constructible() && self.is_trivially_relocatable() {
            result.push(self.copy_wrapper(id));
        }
        if self.is_destructible() && !self.is_trivially_destructible() {
            result.push(self.drop_wrapper(id));
        }
        if self.is_move_constructible() {
            result.push(self.move_wrapper(id));
        }
        result
    }

    pub const fn is_copy_constructible(&self) -> bool {
        self.flags.contains(Flags::IS_COPY_CONSTRUCTIBLE)
    }

    pub const fn is_move_constructible(&self) -> bool {
        self.flags.contains(Flags::IS_MOVE_CONSTRUCTIBLE)
    }

    pub const fn is_trivially_copyable(&self) -> bool {
        self.flags.contains(Flags::IS_TRIVIALLY_COPYABLE)
    }

    pub const fn is_trivially_destructible(&self) -> bool {
        self.flags.contains(Flags::IS_TRIVIALLY_DESTRUCTIBLE)
    }

    pub const fn is_destructible(&self) -> bool {
        self.flags.contains(Flags::IS_DESTRUCTIBLE)
    }

    pub const fn is_trivially_relocatable(&self) -> bool {
        self.flags.contains(Flags::IS_TRIVIALLY_RELOCATABLE)
    }

    fn copy_wrapper(&self, id: u64) -> String {
        format!(
            r#"
extern "C" void ___rpp_clone_{id}(const void* src, void* dst) {{
    rpp::clone_helper<{name}>(src, dst);
}}
"#,
            name = self.name
        )
    }

    fn drop_wrapper(&self, id: u64) -> String {
        format!(
            r#"
extern "C" void ___rpp_drop_{id}(void* ptr) {{
    rpp::drop_helper<{name}>(ptr);
}}
"#,
            name = self.name
        )
    }

    fn move_wrapper(&self, id: u64) -> String {
        format!(
            r#"
extern "C" void ___rpp_move_{id}({name} && src, {name} * dst) {{
    rpp::move_helper<{name}>(std::move(src), dst);
}}
"#,
            name = self.name
        )
    }

    fn copy_impl(&self, ident: &Ident, tokens: &mut TokenStream) {
        quote!(
            impl ::core::marker::Copy for #ident {}
            impl ::core::clone::Clone for #ident {
                fn clone(&self) -> Self { *self }
            }
        )
        .to_tokens(tokens);
    }

    fn move_impl(&self, ident: &Ident, id: u64, tokens: &mut TokenStream) {
        let link_name = LitStr::new(&format!("___rpp_move_{id}"), tokens.span());
        quote!(
            unsafe impl rpp::Move for #ident {
                unsafe fn move_to_ptr(mut value: ::core::pin::Pin<rpp::StackBox<#ident>>, dst: *mut ::core::ffi::c_void) {
                    extern "C" {
                        #[link_name = #link_name]
                        fn fun(src: ::core::pin::Pin<&mut #ident>, dst: *mut ::core::ffi::c_void);
                    }
                    fun(value.as_mut(), dst);
                }
            }
        )
        .to_tokens(tokens);
    }

    fn clone_impl(&self, ident: &Ident, id: u64, tokens: &mut TokenStream) {
        let link_name = LitStr::new(&format!("___rpp_clone_{id}"), tokens.span());
        quote!(
            impl ::core::clone::Clone for #ident {
                fn clone(&self) -> Self {
                    extern "C" {
                        #[link_name = #link_name]
                        fn fun(this: &#ident, result: *mut #ident);
                    }
                    unsafe {
                        let mut result = ::core::mem::MaybeUninit::uninit();
                        fun(self, result.as_mut_ptr());
                        result.assume_init()
                    }
                }
            }
        )
        .to_tokens(tokens);
    }

    fn drop_impl(&self, ident: &Ident, id: u64, tokens: &mut TokenStream) {
        let link_name = LitStr::new(&format!("___rpp_drop_{id}"), tokens.span());
        quote!(
            impl Drop for #ident {
                fn drop(&mut self) {
                    extern "C" {
                        #[link_name = #link_name]
                        fn fun(this: &mut #ident);
                    }
                    unsafe { fun(self); }
                }
            }
        )
        .to_tokens(tokens);
    }
}
