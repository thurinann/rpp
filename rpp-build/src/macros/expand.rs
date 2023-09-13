use std::collections::{BTreeMap, BTreeSet};
use std::fmt;

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::punctuated::Punctuated;
use syn::visit_mut::VisitMut;
use syn::{parse::*, *};

use crate::macros::ty::TyMacro;
use crate::metadata::Metadata;
use crate::{Id, METADATA};

#[derive(Hash, Debug)]
pub struct ExpandMacro {
    bounds: Vec<Bound>,
    condition: Condition,
    body: syn::File,
}

impl ExpandMacro {
    pub fn declare_bounds(&self) -> impl Iterator<Item = String> + '_ {
        self.bounds.iter().map(|x| x.declare())
    }

    pub fn replace(&self, metadata: &Metadata) -> Option<BTreeMap<u64, (syn::File, String)>> {
        if self.bounds.is_empty() {
            let id = self.body.id();
            Some(BTreeMap::from([(
                id,
                (self.body.clone(), self.condition.using(id)),
            )]))
        } else {
            let instances = self
                .bounds
                .iter()
                .filter_map(|x| metadata.instances.get(&x.ty.id()))
                .collect::<Vec<_>>();
            if instances.len() == self.bounds.len() {
                let mut result = BTreeMap::default();
                for ids in cartesian_product(&instances) {
                    let mut condition = self.condition.clone();
                    let mut body = self.body.clone();
                    for (index, id) in ids.iter().enumerate() {
                        let ident = &self.bounds[index].ident;
                        let mut replacer = Replacer::new(ident, id);
                        replacer.visit_file_mut(&mut body);
                        condition.replace(ident, id);
                    }
                    let id = body.id();
                    result
                        .entry(id)
                        .or_insert_with(|| (body, condition.using(id)));
                }
                Some(result)
            } else {
                None
            }
        }
    }
}

impl Parse for ExpandMacro {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut content;
        let bounds = if input.parse::<Token!(for)>().is_ok() {
            parenthesized!(content in input);
            Punctuated::<Bound, Token!(,)>::parse_terminated(&content)?
                .into_iter()
                .collect()
        } else {
            Vec::new()
        };
        let condition = match input.parse::<Token!(if)>() {
            Ok(_) => input.parse()?,
            _ => Default::default(),
        };
        braced!(content in input);
        let body = content.parse()?;
        Ok(Self {
            condition,
            bounds,
            body,
        })
    }
}

impl ToTokens for ExpandMacro {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if self.bounds.is_empty() {
            if METADATA
                .conditions
                .get(&self.body.id())
                .copied()
                .unwrap_or_default()
            {
                self.body.to_tokens(tokens);
            }
        } else {
            let instances = self
                .bounds
                .iter()
                .filter_map(|x| METADATA.instances.get(&x.ty.id()))
                .collect::<Vec<_>>();
            if instances.len() == self.bounds.len() {
                for names in cartesian_product(&instances) {
                    let mut body = self.body.clone();
                    for (index, name) in names.iter().enumerate() {
                        let mut visitor = Replacer::new(&self.bounds[index].ident, name);
                        visitor.visit_file_mut(&mut body);
                    }
                    if METADATA
                        .conditions
                        .get(&body.id())
                        .copied()
                        .unwrap_or_default()
                    {
                        body.to_tokens(tokens);
                    }
                }
            }
        }
    }
}

#[derive(Debug)]
struct Replacer<'a> {
    from: String,
    to: &'a str,
}

impl<'a> Replacer<'a> {
    fn new(ident: &Ident, to: &'a str) -> Self {
        Self {
            from: format!("@{ident}@"),
            to,
        }
    }
}

impl<'a> VisitMut for Replacer<'a> {
    fn visit_macro_mut(&mut self, i: &mut Macro) {
        const IDENTS: [&str; 11] = [
            "ty",
            "par_ty",
            "ex",
            "val",
            "var",
            "enum_val",
            "wrap_const",
            "wrap_struct",
            "wrap_enum",
            "wrap_flags",
            "wrap_template",
        ];
        if let Some(tokens) = i
            .path
            .get_ident()
            .filter(|x| IDENTS.iter().any(|ident| x == ident))
            .map(|_| i.tokens.to_string().replace(&self.from, self.to))
            .and_then(|s| syn::parse_str(&s).ok())
        {
            i.tokens = tokens;
        } else {
            visit_mut::visit_macro_mut(self, i);
        }
    }
}

#[derive(Hash, Debug)]
struct Bound {
    ident: Ident,
    ty: TyMacro,
}

impl Bound {
    fn declare(&self) -> String {
        self.ty.declaration()
    }
}

impl Parse for Bound {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = input.parse()?;
        input.parse::<Token!(=)>()?;
        let ty = input.parse()?;
        Ok(Self { ident, ty })
    }
}

#[derive(Clone, Hash, Debug)]
struct Condition(String);

impl Default for Condition {
    fn default() -> Self {
        Self("true".to_owned())
    }
}

impl Condition {
    fn replace(&mut self, ident: &Ident, name: &str) {
        self.0 = self.0.replace(&format!("@{ident}@"), name);
    }

    fn using(&self, id: u64) -> String {
        format!("const bool ___rpp_condition_{id} = {self};")
    }
}

impl Parse for Condition {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self(input.parse::<LitStr>()?.value()))
    }
}

impl fmt::Display for Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

fn cartesian_product<'a>(lists: &'a [&'a BTreeSet<String>]) -> Vec<Vec<&'a String>> {
    let mut result = Vec::new();
    let mut iter = lists.iter();
    if let Some(first_list) = iter.next() {
        for i in *first_list {
            result.push(vec![i]);
        }
    }
    for i in iter {
        let mut tmp = Vec::new();
        for r in result {
            for item in *i {
                let mut tmp_item = r.clone();
                tmp_item.push(item);
                tmp.push(tmp_item);
            }
        }
        result = tmp;
    }
    result
}
