use std::str::FromStr;

use proc_macro2::Span;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum RefQualifier {
    LRef,
    RRef,
}

impl FromStr for RefQualifier {
    type Err = syn::Error;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s {
            "lref" => Ok(Self::LRef),
            "rref" => Ok(Self::RRef),
            _ => Err(syn::Error::new(Span::call_site(), "unknown ref qualifier")),
        }
    }
}
