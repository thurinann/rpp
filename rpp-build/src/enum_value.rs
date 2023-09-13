use clang::{Entity, EvaluationResult as ER, TypeKind as TK};
use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use serde::{Deserialize, Serialize};

use crate::ty::Type;
use crate::Id;

#[derive(Clone, PartialEq, PartialOrd, Hash, Debug, Serialize, Deserialize)]
pub struct EnumValue(Type, i128);

impl EnumValue {
    pub fn parse(e: Entity<'_>) -> Option<Self> {
        let ty = e.get_type().map(|x| x.get_canonical_type())?;
        if ty.get_kind() == TK::Enum {
            let ty = Type::parse(ty)?;
            match e.evaluate() {
                Some(ER::SignedInteger(v)) => return Some(Self(ty, v as _)),
                Some(ER::UnsignedInteger(v)) => return Some(Self(ty, v as _)),
                _ => (),
            }
        }
        None
    }
}

impl ToTokens for EnumValue {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ty = &self.0;
        let ident = format_ident!("VAL_{}", self.1.id());
        quote!(#ty::#ident).to_tokens(tokens);
    }
}
