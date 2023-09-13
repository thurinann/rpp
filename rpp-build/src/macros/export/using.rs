use syn::parse::{Parse, ParseStream, Result};
use syn::{Ident, LitStr, Token};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Using {
    ident: Ident,
    ty: String,
}

impl Using {
    pub fn in_header(&self) -> String {
        format!("using {} = {};", self.ident, self.ty)
    }
}

impl Parse for Using {
    fn parse(input: ParseStream) -> Result<Self> {
        input.parse::<Token!(type)>()?;
        let ident = input.parse()?;
        input.parse::<Token!(=)>()?;
        let ty = input.parse::<LitStr>()?.value();
        Ok(Self { ident, ty })
    }
}
