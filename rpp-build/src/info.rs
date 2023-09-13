mod enumeration;
mod member;
mod structure;

use clang::{Entity, TypeKind as TK};
use proc_macro2::TokenStream;
use serde::{Deserialize, Serialize};

use crate::ty::Flags;

use self::enumeration::EnumInfo;
use self::member::ClassMemberInfo;
use self::structure::StructInfo;

#[derive(Clone, Debug, Serialize, Deserialize)]
pub enum Info {
    Struct(StructInfo),
    Enum(EnumInfo),
    ClassMember(ClassMemberInfo),
}

impl Info {
    pub fn parse(e: Entity<'_>) -> Option<Self> {
        let ty = e
            .get_child(Flags::TYPE_INDEX)?
            .get_typedef_underlying_type()?
            .get_canonical_type();
        match ty.get_kind() {
            TK::Enum => Some(Self::Enum(EnumInfo::parse(e)?)),
            TK::Record => Some(Self::Struct(StructInfo::parse(e)?)),
            _ => Some(Self::ClassMember(ClassMemberInfo::parse(e)?)),
        }
    }

    pub fn to_tokens(&self, id: u64, tokens: &mut TokenStream) {
        match self {
            Self::Struct(v) => v.to_tokens(id, tokens),
            Self::Enum(v) => v.to_tokens(id, tokens),
            Self::ClassMember(v) => v.to_tokens(id, tokens),
        }
    }

    pub fn wrappers(&self, id: u64) -> Vec<String> {
        match self {
            Self::Struct(v) => v.wrappers(id),
            _ => Default::default(),
        }
    }

    pub const fn is_copy_constructible(&self) -> bool {
        match self {
            Self::Struct(v) => v.is_copy_constructible(),
            _ => true,
        }
    }

    pub const fn is_move_constructible(&self) -> bool {
        match self {
            Self::Struct(v) => v.is_move_constructible(),
            _ => true,
        }
    }

    pub const fn is_trivially_copyable(&self) -> bool {
        match self {
            Self::Struct(v) => v.is_trivially_copyable(),
            _ => true,
        }
    }

    pub const fn is_trivially_destructible(&self) -> bool {
        match self {
            Self::Struct(v) => v.is_trivially_destructible(),
            _ => true,
        }
    }

    pub const fn is_destructible(&self) -> bool {
        match self {
            Self::Struct(v) => v.is_destructible(),
            _ => true,
        }
    }

    pub const fn is_trivially_relocatable(&self) -> bool {
        match self {
            Self::Struct(v) => v.is_trivially_relocatable(),
            _ => true,
        }
    }
}
