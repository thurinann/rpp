use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream, Result};
use syn::{Block, Token};

use crate::macros::ty::TyMacro;

mod kw {
    syn::custom_keyword!(Clone);
    syn::custom_keyword!(Move);
    syn::custom_keyword!(Drop);
    syn::custom_keyword!(Default);
    syn::custom_keyword!(Unsized);
    syn::custom_keyword!(default);
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum MoveQualifier {
    Default,
    Block(Block),
}

impl Parse for MoveQualifier {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.parse::<kw::default>().is_ok() {
            Ok(Self::Default)
        } else {
            Ok(Self::Block(input.parse()?))
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Derive {
    Move(Option<MoveQualifier>),
    Clone(bool),
    Drop(bool),
    Default,
    Unsized,
}

impl Derive {
    pub fn external(&self, class: &str, pid: u64) -> Option<String> {
        match self {
            Self::Move(Some(MoveQualifier::Block(_))) | Self::Move(None) => {
                let ident = format!("___rpp_rust_move_{pid}");
                let args = format!("{class} *");
                Some(format!(r#"extern "C" void {ident}({args});"#))
            }
            Self::Clone(false) => {
                let ident = format!("___rpp_rust_clone_{pid}");
                let args = format!("{class} *, {class} const&");
                Some(format!(r#"extern "C" void {ident}({args});"#))
            }
            Self::Drop(false) => {
                let ident = format!("___rpp_rust_drop_{pid}");
                let args = format!("{class} *");
                Some(format!(r#"extern "C" void {ident}({args});"#))
            }
            Self::Default => {
                let ident = format!("___rpp_rust_default_{pid}");
                let args = format!("{class} *");
                Some(format!(r#"extern "C" void {ident}({args});"#))
            }
            _ => None,
        }
    }

    pub fn in_header(&self, name: &str) -> Option<String> {
        match self {
            Self::Move(None) | Self::Move(Some(MoveQualifier::Block(_))) => Some(format!(
                r#"
{name}({name} && other) noexcept;
{name} & operator=({name} && other) & noexcept;
"#
            )),
            Self::Move(Some(MoveQualifier::Default)) => Some(format!(
                r#"
{name}({name} && other) noexcept = default;
{name} & operator=({name} && other) & noexcept = default;
"#
            )),
            Self::Clone(true) => Some(format!(
                r#"
{name}({name} const& other) noexcept = default;
{name} & operator=({name} const& other) & noexcept = default;
"#
            )),
            Self::Clone(false) => Some(format!(
                r#"
{name}({name} const& other) noexcept;
{name} & operator=({name} const& other) & noexcept;
"#
            )),
            Self::Drop(true) => Some(format!("~{name}() noexcept = default;")),
            Self::Drop(false) => Some(format!("~{name}() noexcept;")),
            Self::Default => Some(format!("{name}() noexcept;")),
            _ => None,
        }
    }

    pub fn in_source(&self, namespace: &str, name: &str, pid: u64) -> Option<String> {
        match self {
            Self::Move(Some(MoveQualifier::Block(_))) | Self::Move(None) => Some(format!(
                r#"
{namespace}::{name}::{name}({namespace}::{name} && other) noexcept : repr(other.repr) {{
    ___rpp_rust_move_{pid}(&other);
}}

{namespace}::{name} & {namespace}::{name}::operator=({namespace}::{name} && other) & noexcept {{
    ___rpp_rust_drop_{pid}(this);
    this->repr = other.repr;
    ___rpp_rust_move_{pid}(&other);
    return *this;
}}
"#
            )),
            Self::Clone(false) => Some(format!(
                r#"
{namespace}::{name}::{name}({namespace}::{name} const& other) noexcept {{
    ___rpp_rust_clone_{pid}(this, other);
}}

{namespace}::{name} & {namespace}::{name}::operator=({namespace}::{name} const& other) & noexcept {{
    if (*this != other) {{
        ___rpp_rust_drop_{pid}(this);
        ___rpp_rust_clone_{pid}(this, other);
    }}
    return *this;
}}
"#
            )),
            Self::Drop(false) => Some(format!(
                r#"
{namespace}::{name}::~{name}() noexcept {{
    ___rpp_rust_drop_{pid}(this);
}}
"#
            )),
            Self::Default => Some(format!(
                r#"
{namespace}::{name}::{name}() noexcept {{
    ___rpp_rust_default_{pid}(this);
}}
"#
            )),
            _ => None,
        }
    }

    pub fn to_tokens(&self, class: &TyMacro, pid: u64) -> Option<TokenStream> {
        match self {
            Self::Move(Some(MoveQualifier::Block(b))) => {
                let ident = format_ident!("___rpp_rust_move_{pid}");
                let args = quote!(this: *mut #class);
                let body = quote!(::core::ptr::write(this, #b););
                Some(quote!(#[no_mangle] extern "C" fn #ident(#args) { unsafe { #body } }))
            }
            Self::Move(None) => {
                let ident = format_ident!("___rpp_rust_move_{pid}");
                let args = quote!(this: *mut #class);
                let body = quote!(::core::ptr::write(this, Default::default()););
                Some(quote!(#[no_mangle] extern "C" fn #ident(#args) { unsafe { #body } }))
            }
            Self::Clone(false) => {
                let ident = format_ident!("___rpp_rust_clone_{pid}");
                let args = quote!(this: *mut #class, other: &#class);
                let body = quote!(::core::ptr::write(this, other.clone()););
                Some(quote!(#[no_mangle] extern "C" fn #ident(#args) { unsafe { #body } }))
            }
            Self::Drop(false) => {
                let ident = format_ident!("___rpp_rust_drop_{pid}");
                let args = quote!(this: *mut #class);
                let body = quote!(::core::ptr::drop_in_place(this););
                Some(quote!(#[no_mangle] extern "C" fn #ident(#args) { unsafe { #body } }))
            }
            Self::Default => {
                let ident = format_ident!("___rpp_rust_default_{pid}");
                let args = quote!(this: *mut #class);
                let body = quote!(::core::ptr::write(this, Default::default()););
                Some(quote!(#[no_mangle] extern "C" fn #ident(#args) { unsafe { #body } }))
            }
            _ => None,
        }
    }
}

impl Parse for Derive {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.parse::<kw::Clone>().is_ok() {
            if input.parse::<Token!(=)>().is_ok() {
                input.parse::<kw::default>()?;
                Ok(Self::Clone(true))
            } else {
                Ok(Self::Clone(false))
            }
        } else if input.parse::<kw::Drop>().is_ok() {
            if input.parse::<Token!(=)>().is_ok() {
                input.parse::<kw::default>()?;
                Ok(Self::Drop(true))
            } else {
                Ok(Self::Drop(false))
            }
        } else if input.parse::<kw::Default>().is_ok() {
            Ok(Self::Default)
        } else if input.parse::<kw::Unsized>().is_ok() {
            Ok(Self::Unsized)
        } else if input.parse::<kw::Move>().is_ok() {
            if input.parse::<Token!(=)>().is_ok() {
                if input.parse::<kw::default>().is_ok() {
                    Ok(Self::Move(Some(MoveQualifier::Default)))
                } else {
                    Ok(Self::Move(Some(input.parse()?)))
                }
            } else {
                Ok(Self::Move(None))
            }
        } else {
            Err(input.error(format!("unknown special method: `{input}`")))
        }
    }
}
