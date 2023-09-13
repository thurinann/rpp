use core::fmt;

use syn::parse::{Parse, ParseStream, Result};
use syn::{Ident, Token};

use super::operator::Operator;

mod kw {
    syn::custom_keyword!(op);
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum MethodIdent {
    Ident(Ident),
    Operator(Operator),
}

impl Parse for MethodIdent {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.parse::<Token!(fn)>().is_ok() {
            Ok(Self::Ident(input.parse()?))
        } else {
            input.parse::<kw::op>()?;
            Ok(Self::Operator(input.parse()?))
        }
    }
}

impl fmt::Display for MethodIdent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident(v) => v.fmt(f),
            Self::Operator(v) => write!(f, "operator {}", v.as_ref()),
        }
    }
}
