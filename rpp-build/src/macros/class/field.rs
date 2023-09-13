use core::fmt;

use syn::parse::{Parse, ParseStream, Result};
use syn::{Ident, LitStr, Token};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct Field {
    ident: Ident,
    ty: String,
}

impl Parse for Field {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = input.parse()?;
        input.parse::<Token!(:)>()?;
        let ty = input.parse::<LitStr>()?.value();
        Ok(Self { ident, ty })
    }
}

impl fmt::Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{} {};", self.ty, self.ident)
    }
}
