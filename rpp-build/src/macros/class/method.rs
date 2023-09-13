use syn::parse::ParseStream;
use syn::punctuated::Punctuated;
use syn::{parenthesized, LitStr, Result, Token};

use crate::macros::rust_macro::RustMacro;

use super::arg::Arg;
use super::attr::Attributes;
use super::method_ident::MethodIdent;
use super::operator::Operator;
use super::receiver::Receiver;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Method {
    attrs: Attributes,
    ident: MethodIdent,
    recv: Receiver,
    args: Vec<Arg>,
    ret: String,
    body: String,
    macros: Vec<(RustMacro, usize, usize)>,
}

impl Method {
    pub fn declarations(&self) -> impl Iterator<Item = String> + '_ {
        self.macros.iter().flat_map(|(x, ..)| x.declarations())
    }

    pub fn in_header(&self) -> String {
        let name = &self.ident;
        let args = self
            .args
            .iter()
            .map(|x| x.in_header())
            .collect::<Vec<_>>()
            .join(", ");
        let prefix = self.recv.prefix();
        let mut suffix = self.recv.suffix().to_owned() + &self.attrs.suffix();
        if self.attrs.overridden() {
            suffix += " override";
        }
        let ret = match &self.ident {
            MethodIdent::Operator(Operator::Convert(_)) => Default::default(),
            _ => self.ret.clone() + " ",
        };
        format!("{prefix}{ret}{name}({args}){suffix};")
    }

    pub fn in_source(&self, namespace: &str, class: &str, id: u64) -> String {
        let ret = match &self.ident {
            MethodIdent::Operator(Operator::Convert(_)) => Default::default(),
            _ => self.ret.clone() + " ",
        };
        let name = &self.ident;
        let args = self
            .args
            .iter()
            .map(|x| x.in_source())
            .collect::<Vec<_>>()
            .join(", ");
        let suffix = self.recv.suffix().to_owned() + &self.attrs.suffix();
        let mut body = self.body.clone();
        for (m, start, end) in &self.macros {
            body = body.replace(&self.body[*start..=*end], &m.call_wrapper(id));
        }
        format!(
            r#"
{ret}{namespace}::{class}::{name}({args}){suffix} {{
{body}
}}
"#
        )
    }

    pub fn macros(&self) -> impl Iterator<Item = &RustMacro> + '_ {
        self.macros.iter().map(|(x, ..)| x)
    }

    pub fn external(&self, pid: u64) -> impl Iterator<Item = String> + '_ {
        self.macros
            .iter()
            .flat_map(move |(x, ..)| x.wrapper(&self.attrs.external(), pid))
    }

    pub fn parse(input: ParseStream, attrs: Attributes) -> Result<Self> {
        let ident = input.parse()?;
        let content;
        parenthesized!(content in input);
        let recv = content.parse()?;
        let _ = content.parse::<Token!(,)>();
        let args = Punctuated::<_, Token!(,)>::parse_terminated(&content)?
            .into_iter()
            .collect();
        let ret = match input.parse::<Token!(->)>() {
            Ok(_) => input.parse::<LitStr>()?.value(),
            _ => "void".to_owned(),
        };
        input.parse::<Token!(=)>()?;
        let body = input.parse::<LitStr>()?.value();
        let macros = RustMacro::collect_from_str(&body);
        Ok(Self {
            attrs,
            ident,
            recv,
            args,
            ret,
            body,
            macros,
        })
    }
}
