use std::hash::Hash;

use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::parse::{Parse, ParseStream, Result};
use syn::{Ident, Token, Visibility};

use super::ty::TyMacro;

#[derive(Hash, Debug)]
pub struct WrapEnumMacro {
    visibility: Visibility,
    ident: Ident,
    ty: TyMacro,
}

impl WrapEnumMacro {
    pub fn ty(&self) -> TyMacro {
        self.ty.clone()
    }

    pub fn repr_ty(&self) -> TyMacro {
        TyMacro::from(format!("std::underlying_type_t<{}>", self.ty))
    }

    pub const fn ident(&self) -> &Ident {
        &self.ident
    }
}

impl Parse for WrapEnumMacro {
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

impl ToTokens for WrapEnumMacro {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let vis = &self.visibility;
        let ident = &self.ident;
        let bind = format_ident!("T{ident}Binding");
        let ty = &self.ty;
        let repr_ty = self.repr_ty();
        quote!(
            #[repr(transparent)]
            #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
            pub struct #ident(#repr_ty);

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
