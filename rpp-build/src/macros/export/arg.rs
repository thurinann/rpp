use syn::parse::{Parse, ParseStream, Result};
use syn::{Ident, LitStr, Token};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Arg {
    ident: Ident,
    ty: String,
    init: Option<String>,
}

impl Arg {
    pub fn in_header(&self) -> String {
        match &self.init {
            Some(init) => format!("{} {} = {init}", self.ty, self.ident),
            _ => format!("{} {}", self.ty, self.ident),
        }
    }

    pub fn in_source(&self) -> String {
        format!("{} {}", self.ty, self.ident)
    }

    pub fn replace(&self, to: impl AsRef<str>) -> Self {
        Self {
            ident: self.ident.clone(),
            ty: self.ty.replace("@Self@", to.as_ref()),
            init: self.init.as_ref().map(|x| x.replace("@Self@", to.as_ref())),
        }
    }
}

impl Parse for Arg {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = input.parse()?;
        input.parse::<Token!(:)>()?;
        let ty = input.parse::<LitStr>()?.value();
        let init = match input.parse::<Token!(=)>() {
            Ok(_) => Some(input.parse::<LitStr>()?.value()),
            _ => Default::default(),
        };
        Ok(Self { ident, ty, init })
    }
}
