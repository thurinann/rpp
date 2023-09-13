mod enum_value;
mod helpers;
pub mod info;
pub mod macros;
mod metadata;
mod parser;
mod ty;
mod value;

use std::collections::hash_map::DefaultHasher;
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::PathBuf;

use anyhow::Result;
use lazy_static::lazy_static;

use crate::metadata::Metadata;
use crate::parser::Parser;

pub const WRAPPERS_NS: &str = "___rpp_wrappers";

pub trait Id {
    fn id(&self) -> u64;
}

impl<T> Id for T
where
    T: Hash,
{
    fn id(&self) -> u64 {
        let mut hasher = DefaultHasher::default();
        self.hash(&mut hasher);
        hasher.finish()
    }
}

lazy_static! {
    pub static ref OUT_DIR: PathBuf = PathBuf::from(env!("OUT_DIR"));
    pub static ref METADATA: Metadata = {
        let f = fs::File::open(OUT_DIR.join("metadata")).expect("Couldn't open metadata file");
        rmp_serde::from_read(&f).expect("Couldn't read metadata")
    };
}

pub fn build() -> Result<()> {
    let conf_path = OUT_DIR.join("config");
    if conf_path.exists() && conf_path.display().to_string().contains("target") {
        fs::remove_dir_all(&conf_path)?;
    }
    let mut parser = Parser::default();
    let code = parser.parse()?;
    let out = OUT_DIR.join("code");
    fs::write(out.join("wrappers.cpp"), code.as_bytes())?;
    let mut files = Vec::new();
    for entry in fs::read_dir(out.as_path())? {
        let entry = entry?;
        let path = entry.path();
        if path.extension().map_or(false, |x| x == "cpp") {
            files.push(path);
        }
    }
    #[cfg(not(feature = "c++20"))]
    let standard = "-std=c++17";
    #[cfg(feature = "c++20")]
    let standard = "-std=c++20";
    cc::Build::new()
        .cpp(true)
        .flag("-w")
        .flag(standard)
        .files(files)
        .includes(parser.include_paths())
        .try_compile("wrappers")?;
    Ok(())
}
