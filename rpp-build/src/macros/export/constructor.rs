use syn::parse::ParseStream;
use syn::punctuated::Punctuated;
use syn::{parenthesized, LitStr, Result, Token};

use crate::macros::rust_macro::RustMacro;

use super::arg::Arg;
use super::attr::Attributes;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Constructor {
    attrs: Attributes,
    args: Vec<Arg>,
    inits: Vec<String>,
    body: String,
    macros: Vec<(RustMacro, usize, usize)>,
}

impl Constructor {
    pub fn replace(&self, to: impl AsRef<str>) -> Self {
        let to = to.as_ref();
        Self {
            attrs: self.attrs.clone(),
            args: self.args.iter().map(|x| x.replace(to)).collect(),
            inits: self.inits.iter().map(|x| x.replace("@Self@", to)).collect(),
            body: self.body.replace("@Self@", to),
            macros: self
                .macros
                .iter()
                .map(|(m, start, end)| (m.replace(to), *start, *end))
                .collect(),
        }
    }

    pub fn declarations(&self) -> impl Iterator<Item = String> + '_ {
        self.macros.iter().flat_map(|(x, ..)| x.declarations())
    }

    pub fn in_header(&self, class: &str) -> String {
        let args = self
            .args
            .iter()
            .map(|x| x.in_header())
            .collect::<Vec<_>>()
            .join(", ");
        let prefix = self.attrs.explicit();
        let suffix = self.attrs.suffix();
        format!("{prefix}{class}({args}){suffix};")
    }

    pub fn in_source(&self, namespace: &str, class: &str, id: u64) -> String {
        let args = self
            .args
            .iter()
            .map(|x| x.in_source())
            .collect::<Vec<_>>()
            .join(", ");
        let suffix = self.attrs.suffix();
        let mut body = self.body.clone();
        for (m, start, end) in &self.macros {
            body = body.replace(&self.body[*start..=*end], &m.call_wrapper(id));
        }
        let mut inits = self.inits.join(", ");
        if !inits.is_empty() {
            inits = format!(" : {inits}");
        }
        format!(
            r#"
{namespace}::{class}::{class}({args}){suffix}{inits} {{
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
        let content;
        parenthesized!(content in input);
        let args = Punctuated::<_, Token!(,)>::parse_terminated(&content)?
            .into_iter()
            .collect();
        let mut inits = Vec::new();
        if input.parse::<Token!(:)>().is_ok() {
            while !(input.is_empty() || input.peek(Token!(=)) || input.peek(Token!(;))) {
                inits.push(input.parse::<LitStr>()?.value());
                let _ = input.parse::<Token!(,)>();
            }
        }
        input.parse::<Token!(=)>()?;
        let body = input.parse::<LitStr>()?.value();
        let macros = RustMacro::collect_from_str(&body);
        Ok(Self {
            attrs,
            args,
            inits,
            body,
            macros,
        })
    }
}
