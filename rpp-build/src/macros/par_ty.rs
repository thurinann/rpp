use std::hash::Hash;

use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream, Result};
use syn::spanned::Spanned;
use syn::{LitInt, LitStr, Token};

use crate::ty::{ExplicitParamType, Type};
use crate::{Id, METADATA};

use super::ty::TyMacro;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct ParTyMacro {
    ty: TyMacro,
    index: usize,
    ept: Option<ExplicitParamType>,
}

impl ParTyMacro {
    pub fn ty(&self) -> TyMacro {
        self.ty.clone()
    }
}

impl Parse for ParTyMacro {
    fn parse(input: ParseStream) -> Result<Self> {
        let ty = input.parse()?;
        input.parse::<Token!(,)>()?;
        let index = input.parse::<LitInt>()?.base10_parse::<usize>()?;
        let _ = input.parse::<Token!(,)>();
        let ept = input.parse::<ExplicitParamType>().ok();
        Ok(Self { ty, index, ept })
    }
}

impl ToTokens for ParTyMacro {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match METADATA.types.get(&self.ty.id()) {
            Some(Type::Struct(_, args, _)) => match args.get(self.index) {
                Some(Some(arg)) => tokens.extend(arg.rust_type(self.ept)),
                _ => {
                    let msg = format!(
                        "couldn't get argument with index `{}` from struct `{}`",
                        self.index, self.ty
                    );
                    let lit = LitStr::new(&msg, tokens.span());
                    quote!(compile_error!(#lit)).to_tokens(tokens)
                }
            },
            Some(_) => {
                let msg = format!("type `{}` is not a struct", self.ty);
                let lit = LitStr::new(&msg, tokens.span());
                quote!(compile_error!(#lit)).to_tokens(tokens)
            }
            _ => {
                let msg = format!("couldn't find type `{}`", self.ty);
                let lit = LitStr::new(&msg, tokens.span());
                quote!(compile_error!(#lit)).to_tokens(tokens)
            }
        }
    }
}
