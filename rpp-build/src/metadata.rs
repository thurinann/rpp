use std::collections::{BTreeMap, BTreeSet, HashSet};

use proc_macro2::TokenStream;
use quote::ToTokens;
use serde::{Deserialize, Serialize};

use crate::enum_value::EnumValue;
use crate::info::Info;
use crate::ty::Type;
use crate::value::Value;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Deserialize, Serialize)]
pub struct TypeInfo {
    pub size: usize,
    pub align: usize,
    pub path: String,
    pub rust: String,
}

#[derive(Default, Debug, Serialize, Deserialize)]
pub struct Metadata {
    pub types: BTreeMap<u64, Type>,
    pub values: BTreeMap<u64, Value>,
    pub enum_values: BTreeMap<u64, EnumValue>,
    pub vars: BTreeSet<u64>,
    pub infos: BTreeMap<u64, Info>,
    pub instances: BTreeMap<u64, BTreeSet<String>>,
    pub conditions: BTreeMap<u64, bool>,
    pub rust_types: BTreeMap<u64, TypeInfo>,
}

impl Metadata {
    pub fn check_condition(&self, id: u64) -> bool {
        self.conditions.get(&id).copied().unwrap_or_default()
    }

    pub fn flag_declarations(&self) -> HashSet<String> {
        self.types
            .values()
            .flat_map(|x| x.flag_declarations())
            .collect()
    }
}

impl ToTokens for Metadata {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.infos
            .iter()
            .for_each(|(id, r)| r.to_tokens(*id, tokens));
    }
}
