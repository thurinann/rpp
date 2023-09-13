use core::fmt;

use syn::parse::{Parse, ParseStream, Result};
use syn::{LitStr, Token};

mod kw {
    syn::custom_keyword!(public);
    syn::custom_keyword!(private);
    syn::custom_keyword!(protected);
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
enum BaseVisibility {
    Public,
    Protected,
    Private,
}

impl Default for BaseVisibility {
    fn default() -> Self {
        Self::Public
    }
}

impl Parse for BaseVisibility {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.parse::<kw::public>().is_ok() {
            Ok(Self::Public)
        } else if input.parse::<kw::protected>().is_ok() {
            Ok(Self::Protected)
        } else {
            input.parse::<kw::private>()?;
            Ok(Self::Private)
        }
    }
}

impl fmt::Display for BaseVisibility {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Public => "public".fmt(f),
            Self::Protected => "protected".fmt(f),
            _ => "private".fmt(f),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct BaseClass {
    vis: BaseVisibility,
    is_virtual: bool,
    path: String,
}

impl Parse for BaseClass {
    fn parse(input: ParseStream) -> Result<Self> {
        let vis = input.parse().unwrap_or_default();
        let is_virtual = input.parse::<Token!(virtual)>().is_ok();
        let path = input.parse::<LitStr>()?.value();
        Ok(Self {
            vis,
            is_virtual,
            path,
        })
    }
}

impl fmt::Display for BaseClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_virtual {
            write!(f, "{} virtual {}", self.vis, self.path)
        } else {
            write!(f, "{} {}", self.vis, self.path)
        }
    }
}
