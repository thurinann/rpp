use std::fmt;

use quote::ToTokens;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Noexcept {
    Common,
    Expr(syn::Expr),
}

impl fmt::Display for Noexcept {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Common => " noexcept".fmt(f),
            Self::Expr(e) => write!(f, " noexcept({})", e.to_token_stream()),
        }
    }
}
