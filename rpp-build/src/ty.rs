use core::fmt;
use std::{collections::BTreeSet, str::FromStr};

use bitflags::bitflags;
use clang::{EntityKind as EK, Type as ClangType, TypeKind as TK};
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use serde::{Deserialize, Serialize};
use syn::parse::{Parse, ParseStream};
use syn::Ident;

use crate::helpers::split_in_path;
use crate::macros::Macros;
use crate::{Id, METADATA};

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Serialize, Deserialize)]
pub enum ExplicitParamType {
    Signed,
    Unsigned,
    Bool,
    Char,
}

impl Parse for ExplicitParamType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let ident = input.parse::<Ident>()?;
        if ident == "s" {
            Ok(Self::Signed)
        } else if ident == "u" {
            Ok(Self::Unsigned)
        } else if ident == "b" {
            Ok(Self::Bool)
        } else if ident == "c" {
            Ok(Self::Char)
        } else {
            Err(input.error(format!("unknown type param identifier: `{ident}`")))
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Serialize, Deserialize)]
pub enum NonTypeParam {
    Signed(i64),
    Unsigned(u64),
    Bool(bool),
    Char(char),
}

impl NonTypeParam {
    pub fn rust_type(&self, ept: Option<ExplicitParamType>) -> TokenStream {
        if let Some(ept) = ept {
            match ept {
                ExplicitParamType::Signed => quote!(rpp::SParam<#self>),
                ExplicitParamType::Unsigned => quote!(rpp::UParam<#self>),
                ExplicitParamType::Bool => quote!(rpp::BParam<#self>),
                ExplicitParamType::Char => quote!(rpp::CParam<#self>),
            }
        } else {
            match self {
                Self::Signed(v) => quote!(rpp::SParam<#v>),
                Self::Unsigned(v) => quote!(rpp::UParam<#v>),
                Self::Bool(v) => quote!(rpp::BParam<#v>),
                Self::Char(v) => quote!(rpp::CParam<#v>),
            }
        }
    }
}

impl fmt::Display for NonTypeParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Signed(v) => Literal::i64_unsuffixed(*v).fmt(f),
            Self::Unsigned(v) => Literal::u64_unsuffixed(*v).fmt(f),
            Self::Bool(v) => v.fmt(f),
            Self::Char(v) => v.fmt(f),
        }
    }
}

impl ToTokens for NonTypeParam {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Signed(v) => Literal::i64_unsuffixed(*v).to_tokens(tokens),
            Self::Unsigned(v) => Literal::u64_unsuffixed(*v).to_tokens(tokens),
            Self::Bool(v) => v.to_tokens(tokens),
            Self::Char(v) => v.to_tokens(tokens),
        }
    }
}

impl FromStr for NonTypeParam {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Ok(v) = s.parse() {
            Ok(Self::Unsigned(v))
        } else if let Ok(v) = s.parse() {
            Ok(Self::Signed(v))
        } else if let Ok(v) = s.parse() {
            Ok(Self::Char(v))
        } else {
            Ok(Self::Bool(s.parse()?))
        }
    }
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Serialize, Deserialize)]
pub enum TemplateParam {
    Type(Type),
    NonType(NonTypeParam),
}

impl TemplateParam {
    pub fn rust_type_str(&self, macros: &[&Macros]) -> Option<String> {
        match self {
            Self::Type(v) => v.rust_type_str(macros),
            Self::NonType(v) => Some(v.to_token_stream().to_string()),
        }
    }

    pub fn rust_type(&self, ept: Option<ExplicitParamType>) -> TokenStream {
        match self {
            Self::Type(v) => v.to_token_stream(),
            Self::NonType(v) => v.rust_type(ept),
        }
    }
}

impl fmt::Display for TemplateParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Type(v) => v.fmt(f),
            Self::NonType(v) => v.fmt(f),
        }
    }
}

impl ToTokens for TemplateParam {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Type(v) => v.to_tokens(tokens),
            Self::NonType(v) => v.to_tokens(tokens),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Serialize, Deserialize)]
pub enum IntegralType {
    Bool,
    CharS,
    CharU,
    SChar,
    UChar,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    ULong,
    LongLong,
    ULongLong,
    Char16,
    #[cfg(feature = "libc")]
    WChar,
}

impl IntegralType {
    pub fn parse(ty: ClangType<'_>) -> Option<Self> {
        match ty.get_kind() {
            TK::Bool => Some(Self::Bool),
            TK::CharS => Some(Self::CharS),
            TK::CharU => Some(Self::CharU),
            TK::SChar => Some(Self::SChar),
            TK::UChar => Some(Self::UChar),
            TK::Short => Some(Self::Short),
            TK::UShort => Some(Self::UShort),
            TK::Int => Some(Self::Int),
            TK::UInt => Some(Self::UInt),
            TK::Long => Some(Self::Long),
            TK::ULong => Some(Self::ULong),
            TK::LongLong => Some(Self::LongLong),
            TK::ULongLong => Some(Self::ULongLong),
            TK::Char16 => Some(Self::Char16),
            #[cfg(feature = "libc")]
            TK::WChar => Some(Self::WChar),
            _ => None,
        }
    }
}

impl fmt::Display for IntegralType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool => "bool".fmt(f),
            Self::CharS | Self::CharU => "char".fmt(f),
            Self::SChar => "signed char".fmt(f),
            Self::UChar => "unsigned char".fmt(f),
            Self::Short => "short".fmt(f),
            Self::UShort => "unsigned short".fmt(f),
            Self::Int => "int".fmt(f),
            Self::UInt => "unsigned int".fmt(f),
            Self::Long => "long".fmt(f),
            Self::ULong => "unsigned long".fmt(f),
            Self::LongLong => "long long".fmt(f),
            Self::ULongLong => "unsigned long long".fmt(f),
            Self::Char16 => "char16_t".fmt(f),
            #[cfg(feature = "libc")]
            Self::WChar => "wchar_t".fmt(f),
        }
    }
}

impl ToTokens for IntegralType {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Bool => quote!(bool),
            Self::CharS | Self::CharU => quote!(::std::os::raw::c_char),
            Self::SChar => quote!(::std::os::raw::c_schar),
            Self::UChar => quote!(::std::os::raw::c_uchar),
            Self::Short => quote!(::std::os::raw::c_short),
            Self::UShort => quote!(::std::os::raw::c_ushort),
            Self::Int => quote!(::std::os::raw::c_int),
            Self::UInt => quote!(::std::os::raw::c_uint),
            Self::Long => quote!(::std::os::raw::c_long),
            Self::ULong => quote!(::std::os::raw::c_ulong),
            Self::LongLong => quote!(::std::os::raw::c_longlong),
            Self::ULongLong => quote!(::std::os::raw::c_ulonglong),
            Self::Char16 => quote!(u16),
            #[cfg(feature = "libc")]
            Self::WChar => quote!(::libc::wchar_t),
        }
        .to_tokens(tokens);
    }
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug, Serialize, Deserialize)]
pub enum Type {
    Void,
    Float,
    Double,
    Integral(IntegralType),
    Struct(String, Vec<Option<TemplateParam>>, BTreeSet<u64>),
    Enum(String, BTreeSet<u64>),
    Ptr { ty: Box<Self>, is_const: bool },
    LRef { ty: Box<Self>, is_const: bool },
    RRef(Box<Self>),
    Function { args: Vec<Self>, ret: Box<Self> },
    ClassMember(String),
}

impl Type {
    pub fn parse(ty: ClangType<'_>) -> Option<Self> {
        match ty.get_kind() {
            TK::Void => Some(Self::Void),
            TK::Float => Some(Self::Float),
            TK::Double => Some(Self::Double),
            TK::Pointer => Self::parse_ptr(ty),
            TK::LValueReference => Self::parse_lref(ty),
            TK::RValueReference => Self::parse_rref(ty),
            TK::Record => Self::parse_record(ty),
            TK::Enum => Self::parse_enum(ty),
            TK::FunctionPrototype => Self::parse_fun(ty),
            TK::MemberPointer => Self::parse_class_member(ty),
            _ => Some(Self::Integral(IntegralType::parse(ty)?)),
        }
    }

    pub const fn is_void(&self) -> bool {
        matches!(self, Self::Void)
    }

    pub const fn is_pointer(&self) -> bool {
        matches!(self, Self::Ptr { .. })
    }

    pub const fn is_rref(&self) -> bool {
        matches!(self, Self::RRef { .. })
    }

    pub fn is_copy_constructible(&self) -> bool {
        match self {
            Self::Struct(..) => METADATA
                .infos
                .get(&self.to_string().id())
                .map_or(false, |x| x.is_copy_constructible()),
            _ => true,
        }
    }

    pub fn is_move_constructible(&self) -> bool {
        match self {
            Self::Struct(..) => METADATA
                .infos
                .get(&self.to_string().id())
                .map_or(false, |x| x.is_move_constructible()),
            _ => true,
        }
    }

    pub fn is_trivially_destructible(&self) -> bool {
        match self {
            Self::Struct(..) => METADATA
                .infos
                .get(&self.to_string().id())
                .map_or(false, |x| x.is_trivially_destructible()),
            _ => true,
        }
    }

    pub fn is_trivially_copyable(&self) -> bool {
        match self {
            Self::Struct(..) => METADATA
                .infos
                .get(&self.to_string().id())
                .map_or(false, |x| x.is_trivially_copyable()),
            _ => true,
        }
    }

    pub fn is_trivially_relocatable(&self) -> bool {
        match self {
            Self::Struct(..) => {
                let id = self.to_string().id();
                METADATA.rust_types.contains_key(&id)
                    || METADATA
                        .infos
                        .get(&id)
                        .map_or_else(|| false, |x| x.is_trivially_relocatable())
            }
            _ => true,
        }
    }

    pub fn flag_declarations(&self) -> impl Iterator<Item = String> + '_ {
        self.structs().into_iter().map(|x| {
            let x = x.to_string();
            format!(
                r#"
namespace ___rpp_info_{id} {{
    using type = {x};
    const uint64_t flags =
        (std::is_trivially_destructible_v<type> << {TRIVIALLY_DESTRUCTIBLE_BIT}) |
        (std::is_trivially_copyable_v<type>     << {TRIVIALLY_COPYABLE_BIT})     |
        (std::is_copy_constructible_v<type>     << {COPY_CONSTRUCTIBLE_BIT})     |
        (std::is_move_constructible_v<type>     << {MOVE_CONSTRUCTIBLE_BIT})     |
        (std::is_destructible_v<type>           << {DESTRUCTIBLE_BIT})           |
        (rpp::is_trivially_relocatable<type>    << {TRIVIALLY_RELOCATABLE_BIT})  |
        0;
}}
"#,
                id = x.id()
            )
        })
    }

    pub fn structs(&self) -> Vec<&Self> {
        let mut result = Vec::new();
        match self {
            Self::Ptr { ty, .. } | Self::LRef { ty, .. } | Self::RRef(ty) => {
                result.append(&mut ty.structs())
            }
            Self::Struct(..) | Self::Enum(..) | Self::ClassMember(..) => result.push(self),
            Self::Function { args, ret } => {
                result.extend(args.iter().flat_map(|x| x.structs()).chain(ret.structs()))
            }
            _ => (),
        }
        result
    }

    fn parse_lref(ty: ClangType) -> Option<Self> {
        let pt = ty.get_pointee_type()?;
        Some(Self::LRef {
            ty: Box::new(Self::parse(pt)?),
            is_const: pt.is_const_qualified(),
        })
    }

    fn parse_rref(ty: ClangType) -> Option<Self> {
        let pt = ty.get_pointee_type()?;
        Some(Self::RRef(Box::new(Self::parse(pt)?)))
    }

    fn parse_ptr(ty: ClangType) -> Option<Self> {
        let pt = ty.get_pointee_type()?;
        match pt.get_kind() {
            TK::FunctionPrototype => Self::parse_fun(pt),
            TK::MemberPointer => Self::parse_class_member(pt),
            _ => Some(Self::Ptr {
                ty: Box::new(Self::parse(pt)?),
                is_const: pt.is_const_qualified(),
            }),
        }
    }

    fn parse_record(ty: ClangType) -> Option<Self> {
        let ct = ty.get_declaration()?.get_type()?.get_canonical_type();
        let mut name = split_by_colon(&ct.get_display_name());
        let last = name.pop()?;
        name.push(last.chars().take_while(|x| *x != '<').collect());
        let name = name.join("::");
        let mut broken = false;
        let mut args = ct
            .get_template_argument_types()
            .unwrap_or_default()
            .into_iter()
            .map(|x| Some(TemplateParam::Type(Self::parse(x?)?)))
            .inspect(|x| broken |= x.is_none())
            .collect::<Vec<_>>();
        if broken {
            let last = last.chars().skip_while(|x| *x != '<').collect::<String>();
            let block = last
                .strip_prefix('<')
                .and_then(|x| x.strip_suffix('>'))
                .unwrap_or(last.as_str());
            for (a, s) in args
                .iter_mut()
                .zip(split_by_comma(block).into_iter())
                .filter(|(x, _)| x.is_none())
            {
                *a = s.parse().map(TemplateParam::NonType).ok();
            }
        }
        Some(Self::Struct(name, args, Self::get_inheritance_tree(ty)?))
    }

    fn parse_enum(ty: ClangType) -> Option<Self> {
        let name = ty
            .get_declaration()?
            .get_type()?
            .get_canonical_type()
            .get_display_name();
        Some(Self::Enum(name, Self::get_inheritance_tree(ty)?))
    }

    fn parse_fun(ty: ClangType) -> Option<Self> {
        Some(Self::Function {
            args: ty
                .get_argument_types()?
                .into_iter()
                .map(Self::parse)
                .collect::<Option<_>>()?,
            ret: Box::new(Self::parse(ty.get_result_type()?)?),
        })
    }

    fn parse_class_member(ty: ClangType) -> Option<Self> {
        let name = ty.get_display_name();
        Some(Self::ClassMember(name))
    }

    fn get_inheritance_tree(ty: ClangType) -> Option<BTreeSet<u64>> {
        let mut result = BTreeSet::new();
        let id = split_in_path(ty.get_canonical_type().get_display_name()).id();
        result.insert(id);
        for mut base in ty
            .get_declaration()?
            .get_children()
            .into_iter()
            .filter(|x| x.get_kind() == EK::BaseSpecifier)
            .filter_map(|x| x.get_type())
            .map(|x| x.get_canonical_type())
            .filter_map(Self::get_inheritance_tree)
        {
            result.append(&mut base);
        }
        Some(result)
    }

    pub fn rust_type_str(&self, macros: &[&Macros]) -> Option<String> {
        match self {
            Self::Void | Self::Integral(_) | Self::Float | Self::Double => {
                Some(self.to_token_stream().to_string())
            }
            Self::Ptr { ty, is_const: true } => ty
                .rust_type_str(macros)
                .map(|x| quote!(*const #x).to_string()),
            Self::Ptr { ty, .. } => ty
                .rust_type_str(macros)
                .map(|x| quote!(*mut #x).to_string()),
            Self::LRef { ty, is_const: true } => {
                ty.rust_type_str(macros).map(|x| quote!(&#x).to_string())
            }
            Self::LRef { ty, .. } => ty
                .rust_type_str(macros)
                .map(|x| quote!(::core::pin::Pin<&mut #x>).to_string()),
            Self::RRef(ty) => ty
                .rust_type_str(macros)
                .map(|x| quote!(::core::pin::Pin<rpp::StackBox<#x>>).to_string()),
            Self::Struct(path, args, ..) => {
                for m in macros.iter().flat_map(|x| x.exports()) {
                    if *path == m.ty().to_string() {
                        let path = if m.is_unsized() {
                            format!("&{}", m.rust_path())
                        } else {
                            m.rust_path()
                        };
                        return if args.is_empty() {
                            Some(path)
                        } else {
                            let args = args
                                .iter()
                                .map(|x| x.as_ref().and_then(|x| x.rust_type_str(macros)))
                                .collect::<Option<Vec<_>>>()?
                                .join(", ");
                            Some(format!("{path}<{args}>"))
                        };
                    }
                }
                None
            }
            _ => None,
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Self::Void
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Void => "void".fmt(f),
            Self::Integral(v) => v.fmt(f),
            Self::Float => "float".fmt(f),
            Self::Double => "double".fmt(f),
            Self::Ptr { ty, is_const: true } => write!(f, "{ty} const*"),
            Self::Ptr { ty, .. } => write!(f, "{ty} *"),
            Self::LRef { ty, is_const: true } => write!(f, "{ty} const&"),
            Self::LRef { ty, .. } => write!(f, "{ty} &"),
            Self::RRef(ty) => write!(f, "{ty} &&"),
            Self::Struct(name, args, ..) => {
                if args.is_empty() {
                    name.fmt(f)
                } else {
                    let args = args
                        .iter()
                        .map(|x| x.as_ref().unwrap().to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    // let mut file = std::fs::OpenOptions::new()
                    //     .create(true)
                    //     .write(true)
                    //     .append(true)
                    //     .open("/home/lliakaji/Documents/Coding/out.txt")
                    //     .unwrap();
                    // use std::io::Write;
                    // writeln!(file, "{name}<{args}>");
                    write!(f, "{name}<{args}>")
                }
            }
            Self::Enum(name, _) | Self::ClassMember(name) => name.fmt(f),
            Self::Function { args, ret } => write!(
                f,
                "{ret}(*)({})",
                args.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl ToTokens for Type {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Void => quote!(::core::ffi::c_void),
            Self::Integral(v) => quote!(#v),
            Self::Float => quote!(::std::os::raw::c_float),
            Self::Double => quote!(::std::os::raw::c_double),
            Self::Ptr { ty, is_const: true } => quote!(*const #ty),
            Self::Ptr { ty, .. } => quote!(*mut #ty),
            Self::LRef { ty, is_const: true } => quote!(&#ty),
            Self::LRef { ty, .. } => quote!(::core::pin::Pin<&mut #ty>),
            Self::RRef(ty) => quote!(::core::pin::Pin<rpp::StackBox<#ty>>),
            Self::Struct(..) => {
                let id = self.to_string().id();
                if let Some(info) = METADATA.rust_types.get(&id) {
                    syn::parse_str::<syn::Type>(&info.rust)
                        .ok()
                        .into_token_stream()
                } else {
                    let ident = format_ident!("RPPStruct{}", id);
                    quote!(rpp::#ident)
                }
            }
            Self::Enum(name, _) => {
                let ident = format_ident!("RPPEnum{}", name.id());
                quote!(rpp::#ident)
            }
            Self::ClassMember(name) => {
                let ident = format_ident!("RPPClassMember{}", name.id());
                quote!(rpp::#ident)
            }
            Self::Function { args, ret } => match **ret {
                Self::Void => quote!(extern "C" fn(#(#args,)*)),
                _ => quote!(extern "C" fn(#(#args,)*) -> #ret),
            },
        }
        .to_tokens(tokens);
    }
}

bitflags! {
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Debug, Serialize, Deserialize)]
    pub struct Flags: u32 {
        const IS_TRIVIALLY_DESTRUCTIBLE = 0b0000000001;
        const IS_TRIVIALLY_COPYABLE     = 0b0000000010;
        const IS_COPY_CONSTRUCTIBLE     = 0b0000000100;
        const IS_DESTRUCTIBLE           = 0b0000001000;
        const IS_MOVE_CONSTRUCTIBLE     = 0b0000010000;
        const IS_TRIVIALLY_RELOCATABLE  = 0b0000100000;
    }
}

impl Flags {
    pub const TYPE_INDEX: usize = 0;
    pub const FLAGS_INDEX: usize = 1;
}

const TRIVIALLY_DESTRUCTIBLE_BIT: u32 = log2(Flags::IS_TRIVIALLY_DESTRUCTIBLE.bits());
const TRIVIALLY_COPYABLE_BIT: u32 = log2(Flags::IS_TRIVIALLY_COPYABLE.bits());
const COPY_CONSTRUCTIBLE_BIT: u32 = log2(Flags::IS_COPY_CONSTRUCTIBLE.bits());
const DESTRUCTIBLE_BIT: u32 = log2(Flags::IS_DESTRUCTIBLE.bits());
const MOVE_CONSTRUCTIBLE_BIT: u32 = log2(Flags::IS_MOVE_CONSTRUCTIBLE.bits());
const TRIVIALLY_RELOCATABLE_BIT: u32 = log2(Flags::IS_TRIVIALLY_RELOCATABLE.bits());

const fn log2(x: u32) -> u32 {
    u32::BITS - x.leading_zeros() - 1
}

fn split_by_comma(s: &str) -> Vec<String> {
    let mut result = Vec::new();
    let mut item = String::new();
    let mut stack = Vec::new();
    for c in s.chars() {
        if c == '<' || c == '(' || c == '[' {
            stack.push(c);
        } else if c == '>' {
            if stack.last().map_or(false, |x| *x == '<') {
                stack.pop();
            }
        } else if c == ')' {
            if stack.last().map_or(false, |x| *x == '(') {
                stack.pop();
            }
        } else if c == ']' {
            if stack.last().map_or(false, |x| *x == ']') {
                stack.pop();
            }
        } else if c == ',' && stack.is_empty() {
            result.push(item.trim().to_owned());
            item.clear();
            continue;
        }
        item.push(c);
    }
    if !item.is_empty() {
        result.push(item.trim().to_owned());
    }
    result
}

fn split_by_colon(s: &str) -> Vec<String> {
    let mut result = Vec::new();
    let mut item = String::new();
    let mut count = 0;
    for c in s.chars() {
        if c == '<' {
            count += 1;
        } else if c == '>' {
            count -= 1
        } else if c == ':' && count == 0 && !item.is_empty() {
            result.push(
                item.strip_prefix(':')
                    .map_or_else(|| item.clone(), |x| x.to_owned()),
            );
            item.clear();
            continue;
        }
        item.push(c);
    }
    if !item.is_empty() {
        result.push(
            item.strip_prefix(':')
                .map_or_else(|| item.clone(), |x| x.to_owned()),
        );
    }
    result
}
