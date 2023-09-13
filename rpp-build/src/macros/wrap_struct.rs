use std::hash::Hash;

use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::parse::{Parse, ParseStream, Result};
use syn::{Ident, Token, Visibility};

use crate::{Id, METADATA};

use super::ty::TyMacro;

#[derive(Hash, Debug)]
pub struct WrapStructMacro {
    visibility: Visibility,
    ident: Ident,
    ty: TyMacro,
}

impl WrapStructMacro {
    pub fn ty(&self) -> TyMacro {
        self.ty.clone()
    }
}

impl Parse for WrapStructMacro {
    fn parse(input: ParseStream) -> Result<Self> {
        let visibility = input.parse()?;
        let ident = input.parse()?;
        input.parse::<Token!(as)>()?;
        let ty = input.parse()?;
        Ok(Self {
            visibility,
            ident,
            ty,
        })
    }
}

impl ToTokens for WrapStructMacro {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let vis = &self.visibility;
        let ident = &self.ident;
        let ty = &self.ty;
        let bind = format_ident!("T{ident}Binding");
        let mut derives = TokenStream::default();
        if let Some(ty) = METADATA.types.get(&self.ty.id()) {
            if ty.is_trivially_copyable() {
                quote!(#[derive(Copy, Clone)]).to_tokens(&mut derives);
            } else if ty.is_copy_constructible() && ty.is_trivially_relocatable() {
                quote!(#[derive(Clone)]).to_tokens(&mut derives);
            }
            if ty.is_move_constructible() {
                quote!(
                    unsafe impl rpp::Move for #ident {
                        unsafe fn move_to_ptr(mut value: ::core::pin::Pin<rpp::StackBox<#ident>>, dst: *mut ::core::ffi::c_void) {
                            <#ty as rpp::Move>::move_to_ptr(::core::mem::transmute(value), dst);
                        }
                    }
                ).to_tokens(tokens);
            }
        }
        quote!(
            #[repr(transparent)]
            #derives
            #vis struct #ident(#ty);

            #vis trait #bind {
                type Type;
            }

            unsafe impl rpp::CppRepr for #ident {
                type Repr = #ty;
            }
        )
        .to_tokens(tokens);
    }
}
