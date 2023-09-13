use syn::parse::{Parse, ParseStream, Result};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Receiver {
    Const,
    Mut,
    Static,
}

impl Receiver {
    pub const fn prefix(&self) -> &str {
        match self {
            Self::Static => "static ",
            _ => "",
        }
    }

    pub const fn suffix(&self) -> &str {
        match self {
            Self::Const => " const",
            _ => "",
        }
    }
}

impl Parse for Receiver {
    fn parse(input: ParseStream) -> Result<Self> {
        match input.parse::<syn::Receiver>() {
            Ok(syn::Receiver {
                reference: Some(_),
                mutability: None,
                ..
            }) => Ok(Self::Const),
            Ok(syn::Receiver {
                reference: Some(_),
                mutability: Some(_),
                ..
            }) => Ok(Self::Mut),
            Ok(syn::Receiver {
                reference: None, ..
            }) => Err(input.error("own receiver in C++ method")),
            _ => Ok(Self::Static),
        }
    }
}
