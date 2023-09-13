mod arg;
pub mod attr;
mod base;
mod constructor;
mod explicit;
mod field;
mod method;
mod method_ident;
mod noexcept;
mod operator;
mod receiver;
mod refq;
mod using;

use std::collections::HashSet;
use std::hash::{Hash, Hasher};

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::parse::{Parse, ParseStream, Result};
use syn::token::{Brace, Paren};
use syn::{braced, Ident, LitStr, Token};

use crate::Id;

use self::base::BaseClass;
use self::constructor::Constructor;
use self::field::Field;
use self::method::Method;
use self::using::Using;

use super::ty::TyMacro;

#[derive(Clone, Debug)]
pub struct ClassMacro {
    namespace: String,
    name: String,
    bases: Vec<BaseClass>,
    constructors: Vec<Constructor>,
    usings: Vec<Using>,
    methods: Vec<Method>,
    fields: Vec<Field>,
}

impl PartialEq for ClassMacro {
    fn eq(&self, other: &Self) -> bool {
        self.ty() == other.ty()
    }
}

impl Eq for ClassMacro {}

impl Hash for ClassMacro {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ty().hash(state);
    }
}

impl ClassMacro {
    pub fn forward(&self) -> String {
        format!("namespace {} {{ class {}; }}", self.namespace, self.name)
    }

    pub fn declarations(&self) -> impl Iterator<Item = String> + '_ {
        self.methods
            .iter()
            .flat_map(|x| x.declarations())
            .chain(self.constructors.iter().flat_map(|x| x.declarations()))
            .chain(std::iter::once(self.ty().declaration()))
    }

    fn ty(&self) -> TyMacro {
        TyMacro::from(format!("{}::{}", self.namespace, self.name))
    }

    pub fn external(&self) -> impl Iterator<Item = String> + '_ {
        let id = self.id();
        self.constructors
            .iter()
            .flat_map(move |x| x.external(id))
            .chain(self.methods.iter().flat_map(move |x| x.external(id)))
    }

    pub fn class_declaration(&self) -> String {
        let body = self
            .usings
            .iter()
            .map(|x| x.in_header())
            .chain(self.methods.iter().map(|x| x.in_header()))
            .chain(self.constructors.iter().map(|x| x.in_header(&self.name)))
            .collect::<Vec<_>>()
            .join("\n");
        let mut bases = self
            .bases
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        if !bases.is_empty() {
            bases = format!(" : {bases}");
        }
        let fields = self
            .fields
            .iter()
            .map(|x| x.to_string())
            .collect::<String>();
        format!(
            r#"namespace {namespace} {{
class {name}{bases} {{
public:
{body}
private:
{fields}
}};
}}"#,
            namespace = self.namespace,
            name = self.name,
        )
    }

    pub fn class_definition(&self) -> String {
        let id = self.id();
        self.constructors
            .iter()
            .map(|x| x.in_source(&self.namespace, &self.name, id))
            .chain(
                self.methods
                    .iter()
                    .map(|x| x.in_source(&self.namespace, &self.name, id)),
            )
            .collect::<Vec<_>>()
            .join("\n")
    }
}

impl Parse for ClassMacro {
    fn parse(input: ParseStream) -> Result<Self> {
        let path = input.parse::<LitStr>()?.value();
        let mut segments = path.split("::").map(|x| x.trim()).collect::<Vec<_>>();
        let name = segments
            .pop()
            .ok_or_else(|| input.error(format!("couldn't get name: `{path}`")))?
            .to_owned();
        if segments.is_empty() {
            return Err(input.error(format!("empty namespace: `{path}`")));
        }
        let namespace = segments.join("::");
        let mut bases = Vec::new();
        if input.parse::<Token!(:)>().is_ok() {
            while !(input.is_empty() || input.peek(Brace)) {
                bases.push(input.parse()?);
                let _ = input.parse::<Token!(,)>();
            }
        }
        let content;
        braced!(content in input);
        let mut methods = Vec::new();
        let mut constructors = Vec::new();
        let mut usings = Vec::new();
        let mut fields = Vec::new();
        while !content.is_empty() {
            let attrs = content.parse()?;
            if let Ok(v) = content.parse::<Using>() {
                usings.push(v);
            } else if content.peek(Ident) && content.peek2(Token!(:)) {
                fields.push(content.parse()?);
            } else if content.peek(Paren) {
                constructors.push(Constructor::parse(&content, attrs)?);
            } else {
                methods.push(Method::parse(&content, attrs)?);
            }
        }
        Ok(Self {
            namespace,
            name,
            bases,
            usings,
            constructors,
            methods,
            fields,
        })
    }
}

impl ToTokens for ClassMacro {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let id = self.id();
        let mut declared = HashSet::new();
        for m in self
            .methods
            .iter()
            .flat_map(|x| x.macros())
            .chain(self.constructors.iter().flat_map(|x| x.macros()))
        {
            let mid = m.id();
            if !declared.contains(&mid) {
                declared.insert(mid);
                m.to_tokens(id, tokens);
            }
        }
    }
}
