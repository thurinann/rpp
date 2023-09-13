mod arg;
mod attr;
mod constructor;
mod derives;
mod explicit;
mod method;
mod method_ident;
mod noexcept;
mod operator;
mod receiver;
mod refq;
mod using;

use core::fmt;
use std::collections::HashSet;
use std::hash::{Hash, Hasher};

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::token::{Brace, Bracket, Paren};
use syn::{braced, bracketed, Ident, LitStr, Token};

use crate::metadata::Metadata;
use crate::Id;

use self::constructor::Constructor;
use self::derives::Derive;
use self::method::Method;
use self::using::Using;

use super::ty::TyMacro;

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub enum Generic {
    ISize(Ident),
    USize(Ident),
    Bool(Ident),
    Char(Ident),
    I8(Ident),
    I16(Ident),
    I32(Ident),
    I64(Ident),
    U8(Ident),
    U16(Ident),
    U32(Ident),
    U64(Ident),
    Type(Ident),
}

impl fmt::Display for Generic {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ISize(v) => write!(f, "rust::isize {v}"),
            Self::USize(v) => write!(f, "rust::usize {v}"),
            Self::Bool(v) => write!(f, "bool {v}"),
            Self::Char(v) => write!(f, "char {v}"),
            Self::I8(v) => write!(f, "rust::i8 {v}"),
            Self::I16(v) => write!(f, "rust::i16 {v}"),
            Self::I32(v) => write!(f, "rust::i32 {v}"),
            Self::I64(v) => write!(f, "rust::i64 {v}"),
            Self::U8(v) => write!(f, "rust::u8 {v}"),
            Self::U16(v) => write!(f, "rust::u16 {v}"),
            Self::U32(v) => write!(f, "rust::u32 {v}"),
            Self::U64(v) => write!(f, "rust::u64 {v}"),
            Self::Type(v) => write!(f, "typename {v}"),
        }
    }
}

impl Parse for Generic {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.parse::<Token!(const)>().is_ok() {
            let ident = input.parse()?;
            input.parse::<Token!(:)>()?;
            match input.parse::<Ident>()?.to_string().as_str() {
                "isize" => Ok(Self::ISize(ident)),
                "usize" => Ok(Self::USize(ident)),
                "i8" => Ok(Self::I8(ident)),
                "i16" => Ok(Self::I16(ident)),
                "i32" => Ok(Self::I32(ident)),
                "i64" => Ok(Self::I64(ident)),
                "u8" => Ok(Self::U8(ident)),
                "u16" => Ok(Self::U16(ident)),
                "u32" => Ok(Self::U32(ident)),
                "u64" => Ok(Self::U64(ident)),
                "char" => Ok(Self::Char(ident)),
                "bool" => Ok(Self::Bool(ident)),
                _ => Err(input.error("type `{ty}` is not allowed here")),
            }
        } else {
            Ok(Self::Type(input.parse()?))
        }
    }
}

#[derive(Clone, Debug)]
pub struct ExportMacro {
    rust: Vec<Ident>,
    namespace: String,
    name: String,
    generics: Vec<Generic>,
    derives: Vec<Derive>,
    constructors: Vec<Constructor>,
    usings: Vec<Using>,
    methods: Vec<Method>,
}

impl PartialEq for ExportMacro {
    fn eq(&self, other: &Self) -> bool {
        self.ty() == other.ty()
    }
}

impl Eq for ExportMacro {}

impl Hash for ExportMacro {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ty().hash(state);
    }
}

impl ExportMacro {
    pub fn replace(&self, to: impl AsRef<str>) -> Self {
        let to = to.as_ref();
        Self {
            rust: self.rust.clone(),
            namespace: self.namespace.clone(),
            name: self.name.clone(),
            generics: self.generics.clone(),
            derives: self.derives.clone(),
            usings: self.usings.clone(),
            constructors: self.constructors.iter().map(|x| x.replace(to)).collect(),
            methods: self.methods.iter().map(|x| x.replace(to)).collect(),
        }
    }

    pub fn rust_path(&self) -> String {
        self.rust
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join("::")
    }

    pub fn is_unsized(&self) -> bool {
        self.derives.contains(&Derive::Unsized)
    }

    pub fn forward(&self) -> String {
        let generics = if self.generics.is_empty() {
            "template <typename T = void>".to_owned()
        } else {
            format!(
                "template <{}>",
                self.generics
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            )
        };
        let usings = self
            .usings
            .iter()
            .map(|x| x.in_header())
            .collect::<String>();
        format!(
            "namespace {} {{ {generics} class {} {{ {usings} }}; }}",
            self.namespace, self.name
        )
    }

    pub fn declarations(&self) -> impl Iterator<Item = String> + '_ {
        self.methods
            .iter()
            .flat_map(|x| x.declarations())
            .chain(self.constructors.iter().flat_map(|x| x.declarations()))
    }

    pub fn external(&self) -> impl Iterator<Item = String> + '_ {
        let id = self.id();
        let ty = self.ty().to_string();
        self.constructors
            .iter()
            .flat_map(move |x| x.external(id))
            .chain(self.methods.iter().flat_map(move |x| x.external(id)))
            .chain(self.derives.iter().filter_map(move |x| x.external(&ty, id)))
    }

    pub fn class_declaration(&self, metadata: Option<&Metadata>) -> String {
        let (align, size) = metadata
            .and_then(|x| x.rust_types.get(&self.ty().id()))
            .map_or((1, 1), |x| (x.align, x.size));
        let body = self
            .methods
            .iter()
            .map(|x| x.in_header())
            .chain(self.constructors.iter().map(|x| x.in_header(&self.name)))
            .chain(self.derives())
            .collect::<Vec<_>>()
            .join("\n");
        format!(
            r#"
template <>
class alignas({align}) {namespace}::{name} {{
public:
{body}
private:
std::array<uint8_t, {size}> repr;
}};
"#,
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
            .chain(
                self.derives
                    .iter()
                    .filter_map(|x| x.in_source(&self.namespace, &self.name, id)),
            )
            .collect::<Vec<_>>()
            .join("\n")
    }

    pub fn ty(&self) -> TyMacro {
        TyMacro::from(format!("{}::{}", self.namespace, self.name))
    }

    fn derives(&self) -> Vec<String> {
        let name = &self.name;
        let mut result = self
            .derives
            .iter()
            .filter_map(|x| x.in_header(name))
            .collect::<Vec<_>>();
        if !self.derives.iter().any(|x| matches!(x, Derive::Clone(_))) {
            result.push(format!(
                r#"
{name}({name} const&) noexcept = delete;
{name} & operator=({name} const& other) & noexcept = delete;
"#
            ));
        }
        if !self.derives.iter().any(|x| matches!(x, Derive::Move(_))) {
            result.push(format!(
                r#"
{name}({name} && other) noexcept = delete;
{name} & operator=({name} && other) & noexcept = delete;
"#
            ));
        }
        if !self.derives.iter().any(|x| matches!(x, Derive::Move(_))) {
            result.push(format!("~{name}() noexcept = delete;"));
        }
        result
    }
}

impl Parse for ExportMacro {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut rust = Vec::new();
        while !input.is_empty() && input.peek(Ident) {
            rust.push(input.parse()?);
            let _ = input.parse::<Token!(::)>();
        }
        let generics = if input.peek(Bracket) {
            let content;
            bracketed!(content in input);
            Punctuated::<_, Token!(, )>::parse_terminated(&content)?
                .into_iter()
                .collect()
        } else {
            Default::default()
        };
        input.parse::<Token!(as)>()?;
        let name = input.parse::<LitStr>()?.value();
        input.parse::<Token!(in)>()?;
        let namespace = input
            .parse::<LitStr>()?
            .value()
            .split("::")
            .map(|x| x.trim())
            .collect::<Vec<_>>()
            .join("::");
        let mut derives = Vec::new();
        if input.parse::<Token!(:)>().is_ok() {
            while !(input.is_empty() || input.peek(Brace)) {
                derives.push(input.parse()?);
                let _ = input.parse::<Token!(,)>();
            }
        }
        let content;
        braced!(content in input);
        let mut methods = Vec::new();
        let mut constructors = Vec::new();
        let mut usings = Vec::new();
        while !content.is_empty() {
            if let Ok(v) = content.parse::<Using>() {
                usings.push(v);
            } else {
                let attrs = content.parse()?;
                if content.peek(Paren) {
                    constructors.push(Constructor::parse(&content, attrs)?);
                } else {
                    methods.push(Method::parse(&content, attrs)?);
                }
            }
        }
        Ok(Self {
            rust,
            namespace,
            name,
            generics,
            derives,
            usings,
            constructors,
            methods,
        })
    }
}

impl ToTokens for ExportMacro {
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
        let class = self.ty();
        tokens.extend(self.derives.iter().filter_map(|x| x.to_tokens(&class, id)));
    }
}
