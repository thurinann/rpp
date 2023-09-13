use std::collections::BTreeSet;

use clang::Entity;
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use serde::{Deserialize, Serialize};

use crate::ty::{Flags, IntegralType};
use crate::Id;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize, Deserialize)]
pub struct EnumVariant(i128);

impl EnumVariant {
    pub fn parse(e: Entity<'_>) -> Option<Self> {
        let (i, u) = e.get_enum_constant_value()?;
        let value = if e
            .get_lexical_parent()?
            .get_enum_underlying_type()?
            .is_signed_integer()
        {
            i as _
        } else {
            u as _
        };
        Some(Self(value))
    }
}

impl ToTokens for EnumVariant {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = format_ident!("VAL_{}", self.0.id());
        let value = Literal::i128_unsuffixed(self.0);
        quote!(pub const #ident: Self = Self(#value);).to_tokens(tokens);
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Serialize, Deserialize)]
pub struct EnumInfo {
    ty: IntegralType,
    variants: BTreeSet<EnumVariant>,
}

impl EnumInfo {
    pub fn parse(e: Entity<'_>) -> Option<Self> {
        let ty = e
            .get_child(Flags::TYPE_INDEX)?
            .get_type()?
            .get_canonical_type();
        let decl = ty.get_declaration()?;
        Some(Self {
            ty: IntegralType::parse(decl.get_enum_underlying_type()?.get_canonical_type())?,
            variants: decl
                .get_children()
                .into_iter()
                .map(EnumVariant::parse)
                .collect::<Option<_>>()?,
        })
    }

    pub fn to_tokens(&self, id: u64, tokens: &mut TokenStream) {
        let ty = &self.ty;
        let var = &self.variants;
        let ident = format_ident!("RPPEnum{id}");
        quote!(
            #[repr(transparent)]
            #[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
            pub struct #ident(pub #ty);
            unsafe impl rpp::CppRepr for #ident {
                type Repr = Self;
            }
            impl #ident {
                #(#var)*
            }
            impl ::core::fmt::Debug for #ident {
                fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                    self.0.fmt(f)
                }
            }
        )
        .to_tokens(tokens);
    }
}
