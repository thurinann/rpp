use syn::parse::{Parse, ParseStream, Result};
use syn::{Attribute, Meta, MetaNameValue};

use super::explicit::Explicit;
use super::noexcept::Noexcept;
use super::refq::RefQualifier;

#[derive(Clone, PartialEq, Eq, Hash, Default, Debug)]
pub struct Attributes {
    noexcept: Option<Noexcept>,
    refq: Option<RefQualifier>,
    volatile: bool,
    explicit: Option<Explicit>,
    other: Vec<Attribute>,
}

impl Attributes {
    pub fn explicit(&self) -> String {
        self.explicit
            .as_ref()
            .map(|x| x.to_string())
            .unwrap_or_default()
    }

    pub fn suffix(&self) -> String {
        let mut result = String::new();
        if self.volatile {
            result += " volatile";
        }
        match self.refq {
            Some(RefQualifier::LRef) => result += " &",
            Some(RefQualifier::RRef) => result += " &&",
            _ => (),
        }
        if let Some(e) = &self.noexcept {
            result += &e.to_string();
        }
        result
    }

    pub fn external(&self) -> String {
        self.noexcept
            .as_ref()
            .map(|x| x.to_string())
            .unwrap_or_default()
    }
}

impl Parse for Attributes {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut result = Self {
            explicit: Some(Explicit::Common),
            noexcept: Some(Noexcept::Common),
            ..Default::default()
        };
        for a in Attribute::parse_outer(input)? {
            if let Meta::Path(path) = &a.meta {
                if let Some(ident) = path.get_ident().map(|x| x.to_string()) {
                    match ident.as_str() {
                        "except" => {
                            result.noexcept = None;
                        }
                        "noexcept" => {
                            result.noexcept.replace(Noexcept::Common);
                        }
                        "volatile" => result.volatile = true,
                        "implicit" => result.explicit = None,
                        i if ["lref", "rref"].contains(&i) => {
                            if result.refq.replace(i.parse()?).is_some() {
                                return Err(input.error("multiple ref qualifiers"));
                            }
                        }
                        _ => result.other.push(a),
                    }
                }
            } else if let Meta::NameValue(MetaNameValue { path, value, .. }) = &a.meta {
                if let Some(ident) = path.get_ident().map(|x| x.to_string()) {
                    match ident.as_str() {
                        "noexcept" => {
                            result.noexcept.replace(Noexcept::Expr(value.clone()));
                        }
                        "explicit" => {
                            result.explicit.replace(Explicit::Expr(value.clone()));
                        }
                        _ => result.other.push(a),
                    }
                }
            }
        }
        Ok(result)
    }
}
