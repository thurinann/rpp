use std::hash::Hash;

use proc_macro2::{Literal, TokenStream};
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream, Result};
use syn::{Ident, Token, Visibility};

use crate::value::Value;
use crate::{Id, METADATA};

use super::val::ValMacro;

#[derive(Hash, Debug)]
pub struct WrapConstMacro {
    visibility: Visibility,
    ident: Ident,
    ty: Option<syn::Type>,
    val: ValMacro,
}

impl WrapConstMacro {
    pub fn val(&self) -> ValMacro {
        self.val.clone()
    }
}

impl Parse for WrapConstMacro {
    fn parse(input: ParseStream) -> Result<Self> {
        let visibility = input.parse()?;
        let ident = input.parse()?;
        let ty = match input.parse::<Token!(:)>() {
            Ok(_) => Some(input.parse()?),
            _ => None,
        };
        input.parse::<Token!(=)>()?;
        let val = input.parse()?;
        Ok(Self {
            visibility,
            ident,
            ty,
            val,
        })
    }
}

impl ToTokens for WrapConstMacro {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let vis = &self.visibility;
        let ident = &self.ident;
        match METADATA.values.get(&self.val.id()) {
            Some(Value::I64(v)) => {
                let v = Literal::i64_unsuffixed(*v);
                if let Some(ty) = &self.ty {
                    quote!(#vis const #ident: #ty = #v;).to_tokens(tokens);
                } else {
                    quote!(#vis const #ident: Self = Self(#v);).to_tokens(tokens);
                }
            }
            Some(Value::U64(v)) => {
                let v = Literal::u64_unsuffixed(*v);
                if let Some(ty) = &self.ty {
                    quote!(#vis const #ident: #ty = #v;).to_tokens(tokens);
                } else {
                    quote!(#vis const #ident: Self = Self(#v);).to_tokens(tokens);
                }
            }
            _ => (),
        }
    }
}
