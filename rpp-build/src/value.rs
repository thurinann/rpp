use std::hash::{Hash, Hasher};

use clang::{Entity, EvaluationResult as ER};
use proc_macro2::{Literal, TokenStream};
use quote::ToTokens;
use serde::{Deserialize, Serialize};
use syn::{spanned::Spanned, LitStr};

#[derive(Clone, PartialEq, PartialOrd, Debug, Serialize, Deserialize)]
pub enum Value {
    I64(i64),
    U64(u64),
    F64(f64),
    Str(String),
}

impl Value {
    pub fn parse(e: Entity<'_>) -> Option<Self> {
        match e.evaluate() {
            Some(ER::SignedInteger(v)) => Some(Self::I64(v)),
            Some(ER::UnsignedInteger(v)) => Some(Self::U64(v)),
            Some(ER::Float(v)) => Some(Self::F64(v)),
            Some(ER::String(v))
            | Some(ER::ObjCString(v))
            | Some(ER::CFString(v))
            | Some(ER::Other(v)) => Some(Self::Str(v.to_str().ok()?.to_owned())),
            _ => None,
        }
    }
}

impl Hash for Value {
    fn hash<H>(&self, state: &mut H)
    where
        H: Hasher,
    {
        match self {
            Self::I64(v) => v.hash(state),
            Self::U64(v) => v.hash(state),
            Self::F64(v) => v.to_bits().hash(state),
            Self::Str(v) => v.hash(state),
        }
    }
}

impl ToTokens for Value {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::I64(v) => Literal::i64_unsuffixed(*v).to_tokens(tokens),
            Self::U64(v) => Literal::u64_unsuffixed(*v).to_tokens(tokens),
            Self::F64(v) => Literal::f64_unsuffixed(*v).to_tokens(tokens),
            Self::Str(v) => LitStr::new(v, tokens.span()).to_tokens(tokens),
        }
    }
}
