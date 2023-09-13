use core::fmt;
use std::hash::Hash;

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream, Result};
use syn::spanned::Spanned;
use syn::LitStr;

use crate::{Id, METADATA};

#[derive(Clone, Hash, Debug)]
pub struct ValMacro(String);

impl Parse for ValMacro {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self(input.parse::<LitStr>()?.value()))
    }
}

impl fmt::Display for ValMacro {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "const auto ___rpp_value_{id} = {val};",
            val = self.0,
            id = self.id(),
        )
    }
}

impl ToTokens for ValMacro {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if let Some(v) = METADATA.values.get(&self.id()) {
            v.to_tokens(tokens)
        } else {
            let msg = format!("couldn't find value `{}`", self.0);
            let lit = LitStr::new(&msg, tokens.span());
            quote!(compile_error!(#lit)).to_tokens(tokens);
        }
    }
}
