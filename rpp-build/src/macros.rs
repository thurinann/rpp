pub mod class;
pub mod enum_val;
pub mod ex;
pub mod expand;
pub mod export;
pub mod par_ty;
mod rust_macro;
pub mod ty;
pub mod val;
pub mod var;
pub mod wrap_const;
pub mod wrap_enum;
pub mod wrap_struct;
pub mod wrap_template;

use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

use anyhow::Result;
use syn::visit::{self, Visit};
use walkdir::WalkDir;

use crate::metadata::Metadata;
use crate::{Id, OUT_DIR};

use self::class::ClassMacro;
use self::enum_val::EnumValMacro;
use self::ex::ExMacro;
use self::expand::ExpandMacro;
use self::export::ExportMacro;
use self::par_ty::ParTyMacro;
use self::ty::TyMacro;
use self::val::ValMacro;
use self::var::VarMacro;
use self::wrap_const::WrapConstMacro;
use self::wrap_enum::WrapEnumMacro;
use self::wrap_struct::WrapStructMacro;

#[derive(Default, Debug)]
pub struct Macros {
    types: BTreeMap<u64, TyMacro>,
    vals: BTreeMap<u64, ValMacro>,
    enum_vals: BTreeMap<u64, EnumValMacro>,
    vars: BTreeMap<u64, VarMacro>,
    execs: BTreeMap<u64, ExMacro>,
    expands: Vec<ExpandMacro>,
    exports: Vec<ExportMacro>,
    classes: Vec<ClassMacro>,
}

impl Macros {
    pub fn exports(&self) -> &[ExportMacro] {
        &self.exports
    }

    pub fn read_types(&mut self, path: impl AsRef<Path>) {
        if let Ok(types) = fs::read_to_string(path.as_ref().join("rppconf").join("types")) {
            for line in types.lines().filter(|x| !x.is_empty()) {
                let ty = TyMacro::from(line.trim());
                self.types.entry(ty.id()).or_insert(ty);
            }
        }
    }

    pub fn make_config(&self, path: impl AsRef<Path>, infos: Option<Vec<String>>) -> Result<()> {
        let id = path.as_ref().display().to_string().id();
        let conf_path = OUT_DIR.join(if infos.is_some() { "export" } else { "config" });
        let rpp_conf_path = conf_path.join("rpp_config");
        let src_path = rpp_conf_path.join("src");
        fs::create_dir_all(&src_path)?;
        let code = if infos.is_some() {
            include_str!("type_info")
        } else {
            include_str!("config")
        };
        fs::write(src_path.join("lib.rs"), code.as_bytes())?;
        fs::write(
            rpp_conf_path.join("Cargo.toml"),
            include_str!("config.toml").as_bytes(),
        )?;
        let src_dir = path.as_ref().join("rppconf");
        WalkDir::new(&src_dir)
            .into_iter()
            .filter_map(Result::ok)
            .map(|x| x.path().display().to_string())
            .for_each(|x| println!("cargo:rerun-if-changed={x}"));
        let out = conf_path.join(format!("rppconf{id}"));
        let dst_dir = out.join("src");
        fs::create_dir_all(&dst_dir)?;
        let code = if let Some(infos) = infos {
            self.make_export_code(infos)
        } else {
            self.make_config_code(&src_dir)
        };
        fs::write(dst_dir.join("lib.rs"), code)?;
        let dependencies = fs::read_to_string(src_dir.join("dependencies")).unwrap_or_default();
        fs::write(
            out.join("Cargo.toml"),
            format!(
                r#"
[package]
name = "rppconf{id}"
version = "0.1.0"
edition = "2021"
[dependencies]
{dependencies}
rpp_config = {{ path = "../rpp_config" }}
"#
            ),
        )?;
        Ok(())
    }

    fn make_config_code(&self, src_dir: impl AsRef<Path>) -> String {
        const DEFAULT: &str = "Default::default()";
        let path = src_dir.as_ref().join("dev_code");
        let dev_code = fs::read_to_string(path).unwrap_or_default();
        let path = src_dir.as_ref().join("link");
        let link = fs::read_to_string(path).unwrap_or_else(|_| DEFAULT.to_owned());
        let path = src_dir.as_ref().join("include_paths");
        let include_paths = fs::read_to_string(path).unwrap_or_else(|_| DEFAULT.to_owned());
        let path = src_dir.as_ref().join("code");
        let code = fs::read_to_string(path).unwrap_or_default();
        format!(
            r####"
use rpp_config::*;
fn include_paths() -> Vec<String> {{ {include_paths} }}
fn link() -> Vec<String> {{ {link} }}
pub fn config() -> Config {{
    Config {{
        code: r###"{code}"###.to_owned(),
        include_paths: include_paths(),
        link: link(),
        dev_code: r###"{dev_code}"###.to_owned(),
    }}
}}
"####
        )
    }

    fn make_export_code(&self, infos: Vec<String>) -> String {
        let export = infos.join(", ");
        format!(
            r####"
use rpp_config::*;
pub fn config() -> Vec<TypeInfo> {{
    vec![{export}]
}}
"####
        )
    }

    pub fn execs(&self) -> impl Iterator<Item = String> + '_ {
        self.execs.values().map(|x| x.to_string())
    }

    pub fn values(&self) -> impl Iterator<Item = String> + '_ {
        self.vals.values().map(|x| x.to_string())
    }

    pub fn enum_values(&self) -> impl Iterator<Item = String> + '_ {
        self.enum_vals.values().map(|x| x.to_string())
    }

    pub fn vars(&self) -> impl Iterator<Item = String> + '_ {
        self.vars.values().map(|x| x.to_string())
    }

    pub fn class_declarations(&self) -> impl Iterator<Item = String> + '_ {
        self.classes.iter().map(|x| x.class_declaration())
    }

    pub fn export_declarations<'a>(
        &'a self,
        metadata: Option<&'a Metadata>,
    ) -> impl Iterator<Item = String> + 'a {
        self.exports
            .iter()
            .map(move |x| x.class_declaration(metadata))
    }

    pub fn class_definitions(&self) -> impl Iterator<Item = String> + '_ {
        self.exports
            .iter()
            .map(|x| x.class_definition())
            .chain(self.classes.iter().map(|x| x.class_definition()))
    }

    pub fn external(&self) -> impl Iterator<Item = String> + '_ {
        self.exports
            .iter()
            .flat_map(|x| x.external())
            .chain(self.classes.iter().flat_map(|x| x.external()))
    }

    pub fn forward(&self) -> impl Iterator<Item = String> + '_ {
        self.exports
            .iter()
            .map(|x| x.forward())
            .chain(self.classes.iter().map(|x| x.forward()))
    }

    pub fn types(&self) -> impl Iterator<Item = String> + '_ {
        self.types
            .values()
            .map(|x| x.declaration())
            .chain(self.enum_vals.values().map(|x| x.ty().declaration()))
            .chain(self.vars.values().map(|x| x.ty().declaration()))
            .chain(self.execs.values().flat_map(|x| x.declarations()))
            .chain(self.exports.iter().flat_map(|x| x.declarations()))
            .chain(self.classes.iter().flat_map(|x| x.declarations()))
    }

    pub fn bounds(&self) -> impl Iterator<Item = String> + '_ {
        self.expands.iter().flat_map(|x| x.declare_bounds())
    }

    pub fn replace<'a>(
        &'a self,
        metadata: &'a Metadata,
    ) -> impl Iterator<Item = (u64, (syn::File, String))> + '_ {
        self.expands
            .iter()
            .filter_map(|x| x.replace(metadata))
            .flatten()
    }
}

impl<'ast> Visit<'ast> for Macros {
    fn visit_macro(&mut self, i: &'ast syn::Macro) {
        if let Some(ident) = i.path.get_ident() {
            if let Some(Ok(v)) = (ident == "ex").then(|| i.parse_body::<ExMacro>()) {
                self.execs.entry(v.id()).or_insert(v);
                return;
            } else if let Some(Ok(v)) = (ident == "ty").then(|| i.parse_body::<TyMacro>()) {
                self.types.entry(v.id()).or_insert(v);
                return;
            } else if let Some(Ok(v)) = (ident == "par_ty").then(|| i.parse_body::<ParTyMacro>()) {
                let v = v.ty();
                self.types.entry(v.id()).or_insert(v);
                return;
            } else if let Some(Ok(v)) = (ident == "val").then(|| i.parse_body::<ValMacro>()) {
                self.vals.entry(v.id()).or_insert(v);
                return;
            } else if let Some(Ok(v)) =
                (ident == "enum_val").then(|| i.parse_body::<EnumValMacro>())
            {
                self.enum_vals.entry(v.id()).or_insert(v);
                return;
            } else if let Some(Ok(v)) = (ident == "var").then(|| i.parse_body::<VarMacro>()) {
                self.vars.entry(v.id()).or_insert(v);
                return;
            } else if let Some(Ok(v)) =
                (ident == "wrap_const").then(|| i.parse_body::<WrapConstMacro>())
            {
                let v = v.val();
                self.vals.entry(v.id()).or_insert(v);
                return;
            } else if let Some(Ok(v)) =
                (ident == "wrap_struct").then(|| i.parse_body::<WrapStructMacro>())
            {
                let v = v.ty();
                self.types.entry(v.id()).or_insert(v);
                return;
            } else if let Some(Ok(v)) = (ident == "wrap_enum" || ident == "wrap_flags")
                .then(|| i.parse_body::<WrapEnumMacro>())
            {
                let ty = v.ty();
                self.types.entry(ty.id()).or_insert(ty);
                let ty = v.repr_ty();
                self.types.entry(ty.id()).or_insert(ty);
                return;
            } else if let Some(Ok(v)) = (ident == "export").then(|| i.parse_body()) {
                self.exports.push(v);
                return;
            } else if let Some(Ok(v)) = (ident == "class").then(|| i.parse_body()) {
                self.classes.push(v);
                return;
            } else if let Some(Ok(v)) = (ident == "expand").then(|| i.parse_body()) {
                self.expands.push(v);
                return;
            }
        }
        visit::visit_macro(self, i);
    }
}
