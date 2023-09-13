use core::fmt;
use std::hash::Hash;

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream, Result};
use syn::spanned::Spanned;
use syn::LitStr;

use crate::{Id, METADATA};

use super::ty::TyMacro;

#[derive(Clone, Hash, Debug)]
pub struct EnumValMacro(String);

impl EnumValMacro {
    pub fn ty(&self) -> TyMacro {
        TyMacro::from(format!("decltype({})", self.0))
    }
}

impl Parse for EnumValMacro {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self(input.parse::<LitStr>()?.value()))
    }
}

impl fmt::Display for EnumValMacro {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{ty} ___rpp_enum_value_{id} = {val};",
            val = self.0,
            ty = self.ty(),
            id = self.id(),
        )
    }
}

impl ToTokens for EnumValMacro {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if let Some(v) = METADATA.enum_values.get(&self.id()) {
            v.to_tokens(tokens)
        } else {
            let msg = format!("couldn't find enum value `{}`", self.0);
            let lit = LitStr::new(&msg, tokens.span());
            quote!(compile_error!(#lit)).to_tokens(tokens);
        }
    }
}
