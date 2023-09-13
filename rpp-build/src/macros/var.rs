use core::fmt;
use std::hash::Hash;

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream, Result};
use syn::spanned::Spanned;
use syn::LitStr;

use crate::{Id, METADATA};

use super::ty::TyMacro;

#[derive(Hash, Debug)]
pub struct VarMacro(String);

impl VarMacro {
    pub fn ty(&self) -> TyMacro {
        TyMacro::from(format!("decltype({})", self.0))
    }

    fn cpp_ident(&self) -> String {
        format!("___rpp_var_{}", self.id())
    }
}

impl Parse for VarMacro {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self(input.parse::<LitStr>()?.value()))
    }
}

impl fmt::Display for VarMacro {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            r#"
extern {ty} {ident};
{ty} {ident} = {val};
"#,
            val = self.0,
            ty = self.ty(),
            ident = self.cpp_ident(),
        )
    }
}

impl ToTokens for VarMacro {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if METADATA.vars.contains(&self.id()) {
            let ty = self.ty();
            let link_name = LitStr::new(&self.cpp_ident(), tokens.span());
            quote!({
                extern "C" {
                    #[link_name = #link_name]
                    static var: #ty;
                }
                unsafe { var }
            })
            .to_tokens(tokens);
        } else {
            let msg = format!("couldn't find variable `{}`", self.0);
            let lit = LitStr::new(&msg, tokens.span());
            quote!(compile_error!(#lit)).to_tokens(tokens);
        }
    }
}
