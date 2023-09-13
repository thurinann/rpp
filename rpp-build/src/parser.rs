use std::collections::btree_map::Entry;
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::fs::{self, create_dir_all};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use anyhow::{anyhow, Context, Result};
use cargo_metadata::MetadataCommand;
use clang::{Clang, Entity, EntityKind as EK, EvaluationResult, Index, Unsaved};
use rmp_serde::Serializer;
use serde::{Deserialize, Serialize};
use syn::visit::Visit;
use walkdir::WalkDir;

use crate::enum_value::EnumValue;
use crate::helpers::split_in_path;
use crate::info::Info;
use crate::macros::Macros;
use crate::metadata::{Metadata, TypeInfo};
use crate::ty::Type;
use crate::value::Value;
use crate::{Id, METADATA, OUT_DIR, WRAPPERS_NS};

#[derive(Default, Debug, Deserialize, Serialize)]
struct Config {
    code: String,
    include_paths: Vec<String>,
    link: Vec<String>,
    dev_code: String,
}

#[derive(Default, Debug)]
pub struct Parser {
    macros: BTreeMap<u64, Macros>,
    configs: BTreeMap<u64, Config>,
    instances: BTreeMap<u64, Macros>,
}

impl Parser {
    pub fn parse(&mut self) -> Result<String> {
        let paths = crate_paths()?;
        for path in &paths {
            let macros = self
                .macros
                .entry(path.display().to_string().id())
                .or_default();
            for s in source_paths(path) {
                println!("cargo:rerun-if-changed={}", s.display());
                let source = fs::read_to_string(s)?;
                let file = syn::parse_str(&source)?;
                macros.visit_file(&file);
            }
            macros.read_types(path);
            macros.make_config(path, None)?;
        }
        self.configs = serde_json::from_str(&parse_configs(&paths, "config")?)?;
        self.configs
            .values()
            .flat_map(|x| &x.link)
            .for_each(|x| println!("{x}"));
        let metadata = self.collect_metadata(&paths)?;
        let mut buf = Vec::new();
        let mut serializer = Serializer::new(&mut buf);
        metadata.serialize(&mut serializer)?;
        fs::write(OUT_DIR.join("metadata"), buf)?;
        Ok(self.release_code(metadata))
    }

    pub fn include_paths(&self) -> Vec<String> {
        let mut result = self
            .configs
            .values()
            .flat_map(|x| x.include_paths.clone())
            .collect::<Vec<_>>();
        result.push(OUT_DIR.join("code").display().to_string());
        result.sort_unstable();
        result.dedup();
        result
    }

    fn args(&self) -> Vec<String> {
        let mut result = self
            .include_paths()
            .iter()
            .map(|x| format!("-I{}", x))
            .collect::<Vec<_>>();
        result.sort_unstable();
        result.dedup();
        result.push("-ferror-limit=0".to_owned());
        #[cfg(not(feature = "c++20"))]
        result.push("-std=c++17".to_owned());
        #[cfg(feature = "c++20")]
        result.push("-std=c++20".to_owned());
        result
    }

    fn collect_metadata(&mut self, paths: &[PathBuf]) -> Result<Metadata> {
        let mut result = Metadata::default();
        let mut code = preprocess(self.dev_code(), self.args())?;
        let clang = Clang::new().map_err(|e| anyhow!("{e}"))?;
        let index = Index::new(&clang, false, false);
        let mut tu = index
            .parser("code.cpp")
            .unsaved(&[Unsaved::new("code.cpp", &code)])
            .arguments(&self.args())
            .parse()?;
        let bounds = parse_bounds(tu.get_entity())?;
        let mut inserted = true;
        let mut conditions = HashSet::new();
        let mut used = HashSet::new();
        while inserted {
            inserted = false;
            for e in tu.get_entity().get_children() {
                match e.get_kind() {
                    EK::TypeAliasDecl => {
                        if let Some(Entry::Vacant(entry)) = e
                            .get_name()
                            .and_then(|x| x.strip_prefix("___rpp_typedef_")?.parse().ok())
                            .map(|x| result.types.entry(x))
                        {
                            if let Some(ty) = e
                                .get_type()
                                .map(|x| x.get_canonical_type())
                                .and_then(Type::parse)
                            {
                                if let Type::Struct(_, _, family) | Type::Enum(_, family) = &ty {
                                    inserted = true;
                                    for bound in
                                        family.iter().filter_map(|x| bounds.get(x)).flatten()
                                    {
                                        result
                                            .instances
                                            .entry(*bound)
                                            .or_default()
                                            .insert(ty.to_string());
                                    }
                                }
                                entry.insert(ty);
                            }
                        }
                    }
                    EK::VarDecl => {
                        if let Some(name) = e.get_name() {
                            if let Some(Entry::Vacant(entry)) = name
                                .strip_prefix("___rpp_value_")
                                .and_then(|x| x.parse().ok())
                                .map(|x| result.values.entry(x))
                            {
                                if let Some(v) = Value::parse(e) {
                                    inserted = true;
                                    entry.insert(v);
                                }
                            } else if let Some(Entry::Vacant(entry)) = name
                                .strip_prefix("___rpp_enum_value_")
                                .and_then(|x| x.parse().ok())
                                .map(|x| result.enum_values.entry(x))
                            {
                                if let Some(v) = EnumValue::parse(e) {
                                    inserted = true;
                                    entry.insert(v);
                                }
                            } else if name
                                .strip_prefix("___rpp_var_")
                                .and_then(|x| x.parse().ok())
                                .map_or(false, |x| result.vars.insert(x))
                            {
                                inserted = true;
                            }
                        }
                    }
                    _ => (),
                }
            }
            if inserted {
                for (id, (body, condition)) in self.macros.values().flat_map(|x| x.replace(&result))
                {
                    if let Entry::Vacant(e) = self.instances.entry(id) {
                        let mut instance = Macros::default();
                        instance.visit_file(&body);
                        instance
                            .types()
                            .chain(instance.values())
                            .chain(instance.enum_values())
                            .chain(instance.vars())
                            .filter(|x| used.insert(x.id()))
                            .for_each(|x| code.push_str(&x));
                        conditions.insert(condition);
                        e.insert(instance);
                    }
                }
            } else {
                let mut rust_types = Vec::new();
                let macros = self.macros().collect::<Vec<_>>();
                for ty in result.types.values().flat_map(|x| x.structs()) {
                    if let Some(rust) = ty.rust_type_str(&macros) {
                        rust_types.push(format!(r#"TypeInfo::new::<{rust}>("{ty}")"#));
                    }
                }
                for path in paths {
                    if let Some(m) = self.macros.get_mut(&path.display().to_string().id()) {
                        m.make_config(path, Some(rust_types.clone()))?;
                    }
                }
                for (_, infos) in serde_json::from_str::<BTreeMap<u64, Vec<TypeInfo>>>(
                    &parse_configs(paths, "export")?,
                )? {
                    result
                        .rust_types
                        .extend(infos.into_iter().map(|x| (x.path.id(), x)));
                }
                code.extend(self.macros().flat_map(|x| x.export_declarations(None)));
                code.extend(conditions.iter().map(|x| x.as_str()));
                code.extend(result.flag_declarations());
            }
            tu = tu.reparse(&[Unsaved::new("code.cpp", &code)])?;
        }
        for e in tu.get_entity().get_children() {
            if let Some((id, c)) = parse_condition(e) {
                result.conditions.entry(id).or_insert(c);
            } else if let Some((id, info)) = parse_info(e) {
                result.infos.entry(id).or_insert(info);
            }
        }
        self.instances.retain(|id, _| result.check_condition(*id));
        Ok(result)
    }

    fn common_code(&self) -> String {
        self.configs
            .values()
            .map(|x| x.code.as_str())
            .chain(std::iter::once(include_str!("code.hpp")))
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn dev_code(&self) -> String {
        let mut bounds = self.macros().flat_map(|x| x.bounds()).collect::<Vec<_>>();
        bounds.sort_unstable();
        bounds.dedup();
        let bounds = format!("namespace $RPP {{ {} }}", bounds.join("\n"));
        std::iter::once(self.common_code())
            .chain(self.macros().flat_map(|x| x.forward()))
            .chain(self.macros().flat_map(|x| x.class_declarations()))
            .chain(self.macros().flat_map(|x| x.types()))
            .chain(self.macros().flat_map(|x| x.values()))
            .chain(self.macros().flat_map(|x| x.enum_values()))
            .chain(self.macros().flat_map(|x| x.vars()))
            .chain(std::iter::once(bounds))
            .chain(self.configs.values().map(|x| x.dev_code.clone()))
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn release_code(&self, metadata: Metadata) -> String {
        let mut seen = HashSet::new();
        let mut wrappers = self
            .macros()
            .chain(self.instances())
            .flat_map(|x| x.execs())
            .chain(METADATA.infos.iter().flat_map(|(id, r)| r.wrappers(*id)))
            .collect::<Vec<_>>();
        wrappers.retain(|x| seen.insert(x.id()));
        let mut rust_wrappers = self
            .macros()
            .chain(self.instances())
            .flat_map(|x| x.external())
            .collect::<Vec<_>>();
        rust_wrappers.retain(|x| seen.insert(x.id()));
        let wrappers = wrappers.join("\n");
        let mut vars = self
            .instances()
            .chain(self.macros())
            .flat_map(|x| x.vars())
            .collect::<Vec<_>>();
        vars.retain(|x| seen.insert(x.id()));
        std::iter::once(self.common_code())
            .chain(
                self.macros()
                    .chain(self.instances())
                    .flat_map(|x| x.forward()),
            )
            .chain(rust_wrappers)
            .chain(
                self.macros()
                    .chain(self.instances())
                    .flat_map(|x| x.export_declarations(Some(&metadata))),
            )
            .chain(
                self.macros()
                    .chain(self.instances())
                    .flat_map(|x| x.class_declarations()),
            )
            .chain(std::iter::once(format!(
                "namespace {WRAPPERS_NS} {{ {wrappers} }}"
            )))
            .chain(vars)
            .chain(
                self.macros()
                    .chain(self.instances())
                    .flat_map(|x| x.class_definitions()),
            )
            .collect::<Vec<_>>()
            .join("\n")
    }

    fn macros(&self) -> impl Iterator<Item = &Macros> + '_ {
        self.macros.values()
    }

    fn instances(&self) -> impl Iterator<Item = &Macros> + '_ {
        self.instances.values()
    }
}

fn preprocess(code: String, args: Vec<String>) -> Result<String> {
    let args = args
        .iter()
        .map(|x| x.as_str())
        .chain(["-x", "c++", "-E", "-"]);
    let mut child = Command::new("clang")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .args(args)
        .spawn()?;
    if let Some(mut child_stdin) = child.stdin.take() {
        child_stdin.write_all(code.as_bytes())?;
    }
    let out = child.wait_with_output()?;
    Ok(String::from_utf8(out.stdout)?)
}

// #[cfg(feature = "qt")]
// fn run_moc(code: &str) -> Result<String> {
//     let mut child = Command::new("moc")
//         .current_dir(OUT_DIR.join("code"))
//         .stdin(Stdio::piped())
//         .stdout(Stdio::piped())
//         .spawn()?;
//     if let Some(mut child_stdin) = child.stdin.take() {
//         child_stdin.write_all(code.as_bytes())?;
//     }
//     Ok(String::from_utf8(child.wait_with_output()?.stdout)?)
// }

fn parse_info(e: Entity) -> Option<(u64, Info)> {
    match e.get_kind() {
        EK::Namespace => Some((
            e.get_name()?
                .strip_prefix("___rpp_info_")?
                .parse::<u64>()
                .ok()?,
            Info::parse(e)?,
        )),
        _ => None,
    }
}

fn parse_condition(e: Entity) -> Option<(u64, bool)> {
    match e.get_kind() {
        EK::VarDecl => Some((
            e.get_name()?
                .strip_prefix("___rpp_condition_")?
                .parse::<u64>()
                .ok()?,
            matches!(e.evaluate(), Some(EvaluationResult::UnsignedInteger(1))),
        )),
        _ => None,
    }
}

fn parse_bounds(e: Entity) -> Result<BTreeMap<u64, BTreeSet<u64>>> {
    fn parse_bound(e: Entity) -> Option<(u64, u64)> {
        let id = e
            .get_name()?
            .strip_prefix("___rpp_typedef_")?
            .parse::<u64>()
            .ok()?;
        let uid = split_in_path(e.get_type()?.get_canonical_type().get_display_name()).id();
        Some((uid, id))
    }
    let mut result = BTreeMap::<u64, BTreeSet<u64>>::new();
    for (uid, id) in e
        .get_children()
        .into_iter()
        .find(|x| x.get_name().map_or(false, |x| x == "$RPP"))
        .context("couldn't find bounds namespace")?
        .get_children()
        .into_iter()
        .filter_map(parse_bound)
    {
        result.entry(uid).or_default().insert(id);
    }
    Ok(result)
}

fn source_paths(root: &Path) -> impl Iterator<Item = PathBuf> {
    WalkDir::new(root.join("src"))
        .into_iter()
        .filter_map(Result::ok)
        .filter(|x| x.path().extension().map_or(false, |x| x == "rs"))
        .map(|x| x.into_path())
}

fn crate_paths() -> Result<Vec<PathBuf>> {
    Ok(OUT_DIR
        .ancestors()
        .map(|x| x.join("Cargo.toml"))
        .find(|x| x.exists())
        .map(|x| MetadataCommand::new().manifest_path(x).exec())
        .transpose()?
        .context("couldn't get metadata")?
        .packages
        .into_iter()
        .map(|x| x.manifest_path)
        .filter_map(|x| Some(x.parent()?.to_path_buf().into_std_path_buf()))
        .filter(|x| x.join("rppconf").exists())
        .collect())
}

fn parse_configs(paths: &[PathBuf], conf_dir: &str) -> Result<String> {
    let mut dependencies = String::new();
    let mut calls = String::new();
    for path in paths {
        let id = path.display().to_string().id();
        dependencies += &format!("rppconf{id} = {{ path = \"rppconf{id}\" }}\n");
        calls += &format!("({id}u64, rppconf{id}::config()),");
    }
    let conf_path = OUT_DIR.join(conf_dir);
    let src_path = conf_path.join("src");
    let toml_path = conf_path.join("Cargo.toml");
    let code_path = OUT_DIR.join("code");
    if code_path.exists() && code_path.display().to_string().contains("target") {
        fs::remove_dir_all(&code_path)?;
    }
    create_dir_all(&src_path)?;
    create_dir_all(code_path)?;
    let toml = format!(
        r#"
[package]
name = "config"
version = "0.1.0"
edition = "2021"

[dependencies]
serde_json = "*"
serde = "*"
{dependencies}
"#
    );
    let src = format!(
        r#"
use serde_json::Result;
fn main() -> Result<()> {{
    let configs = std::collections::BTreeMap::from([{calls}]);
    println!("{{}}", serde_json::to_string(&configs)?);
    Ok(())
}}
"#
    );
    fs::write(&toml_path, toml.as_bytes())?;
    fs::write(src_path.join("main.rs"), src.as_bytes())?;
    let toml_path = toml_path.display().to_string();
    let out = Command::new("cargo")
        .args(["run", "--manifest-path", toml_path.as_str()])
        .output()?;
    Ok(String::from_utf8(out.stdout)?)
}
