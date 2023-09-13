use std::fmt::Display;
use std::hash::{Hash, Hasher};

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream, Result};
use syn::spanned::Spanned;
use syn::LitStr;

use crate::{Id, METADATA};

#[derive(Clone, Debug)]
pub struct TyMacro(String);

impl PartialEq for TyMacro {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for TyMacro {}

impl Hash for TyMacro {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.hash(state);
    }
}

impl Default for TyMacro {
    fn default() -> Self {
        Self("void".to_string())
    }
}

impl TyMacro {
    pub fn replace(&self, from: impl AsRef<str>, to: impl AsRef<str>) -> Self {
        Self(self.0.replace(from.as_ref(), to.as_ref()))
    }

    pub fn declaration(&self) -> String {
        format!("using ___rpp_typedef_{} = {};", self.id(), self)
    }

    pub fn is_trivially_copyable(&self) -> bool {
        METADATA
            .types
            .get(&self.id())
            .map_or(false, |x| x.is_trivially_copyable())
    }

    pub fn is_trivially_relocatable(&self) -> bool {
        METADATA
            .types
            .get(&self.id())
            .map_or(false, |x| x.is_trivially_relocatable())
    }

    pub fn is_void(&self) -> bool {
        METADATA
            .types
            .get(&self.id())
            .map_or(false, |x| x.is_void())
    }

    pub fn is_pointer(&self) -> bool {
        METADATA
            .types
            .get(&self.id())
            .map_or(false, |x| x.is_pointer())
    }

    pub fn is_rref(&self) -> bool {
        METADATA
            .types
            .get(&self.id())
            .map_or(false, |x| x.is_rref())
    }
}

impl<T> From<T> for TyMacro
where
    T: AsRef<str>,
{
    fn from(value: T) -> Self {
        Self(value.as_ref().to_owned())
    }
}

impl Display for TyMacro {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Parse for TyMacro {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self(input.parse::<LitStr>()?.value()))
    }
}

impl ToTokens for TyMacro {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match METADATA.types.get(&self.id()) {
            Some(ty) => ty.to_tokens(tokens),
            _ => {
                let msg = format!("couldn't find type `{}`", self.0);
                let lit = LitStr::new(&msg, tokens.span());
                quote!(compile_error!(#lit)).to_tokens(tokens)
            }
        }
    }
}
