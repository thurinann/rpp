use std::fmt;

use quote::ToTokens;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Explicit {
    Common,
    Expr(syn::Expr),
}

impl fmt::Display for Explicit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Common => "explicit ".fmt(f),
            Self::Expr(e) => write!(f, "explicit({}) ", e.to_token_stream()),
        }
    }
}
