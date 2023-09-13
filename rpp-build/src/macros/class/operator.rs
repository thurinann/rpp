use syn::parse::{Parse, ParseStream, Result};
use syn::token::{Bracket, Paren};
use syn::{bracketed, parenthesized, LitStr, Token};

mod kw {
    syn::custom_keyword!(new);
    syn::custom_keyword!(delete);
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Operator {
    New,
    Delete,
    NewArray,
    DeleteArray,
    Inc,
    Dec,
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    NotEq,
    Gt,
    GtOrEq,
    Lt,
    LtOrEq,
    ThreeWay,
    Not,
    And,
    Or,
    BitNot,
    BitAnd,
    BitOr,
    BitXor,
    LShift,
    RShift,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    LShiftAssign,
    RShiftAssign,
    Index,
    Deref,
    DerefMember,
    Call,
    Comma,
    Convert(String),
}

impl Parse for Operator {
    fn parse(input: ParseStream) -> Result<Self> {
        if input.parse::<kw::new>().is_ok() {
            if input.peek(Bracket) {
                let _content;
                bracketed!(_content in input);
                Ok(Self::NewArray)
            } else {
                Ok(Self::New)
            }
        } else if input.parse::<kw::delete>().is_ok() {
            if input.peek(Bracket) {
                let _content;
                bracketed!(_content in input);
                Ok(Self::DeleteArray)
            } else {
                Ok(Self::Delete)
            }
        } else if input.parse::<Token!(+=)>().is_ok() {
            Ok(Self::AddAssign)
        } else if input.parse::<Token!(-=)>().is_ok() {
            Ok(Self::SubAssign)
        } else if input.parse::<Token!(*=)>().is_ok() {
            Ok(Self::MulAssign)
        } else if input.parse::<Token!(/=)>().is_ok() {
            Ok(Self::DivAssign)
        } else if input.parse::<Token!(%=)>().is_ok() {
            Ok(Self::ModAssign)
        } else if input.parse::<Token!(+)>().is_ok() {
            if input.parse::<Token!(+)>().is_ok() {
                Ok(Self::Inc)
            } else {
                Ok(Self::Add)
            }
        } else if input.parse::<Token!(->)>().is_ok() {
            if input.parse::<Token!(*)>().is_ok() {
                Ok(Self::Deref)
            } else {
                Ok(Self::DerefMember)
            }
        } else if input.parse::<Token!(-)>().is_ok() {
            if input.parse::<Token!(-)>().is_ok() {
                Ok(Self::Dec)
            } else {
                Ok(Self::Sub)
            }
        } else if input.parse::<Token!(==)>().is_ok() {
            Ok(Self::Eq)
        } else if input.parse::<Token!(=)>().is_ok() {
            Ok(Self::Assign)
        } else if input.parse::<Token!(!=)>().is_ok() {
            Ok(Self::NotEq)
        } else if input.parse::<Token!(!)>().is_ok() {
            Ok(Self::Not)
        } else if input.peek(Bracket) {
            let _content;
            bracketed!(_content in input);
            Ok(Self::Index)
        } else if input.parse::<Token!(*)>().is_ok() {
            Ok(Self::Mul)
        } else if input.parse::<Token!(/)>().is_ok() {
            Ok(Self::Div)
        } else if input.parse::<Token!(%)>().is_ok() {
            Ok(Self::Mod)
        } else if input.parse::<Token!(&=)>().is_ok() {
            Ok(Self::BitAndAssign)
        } else if input.parse::<Token!(&&)>().is_ok() {
            Ok(Self::And)
        } else if input.parse::<Token!(&)>().is_ok() {
            Ok(Self::BitAnd)
        } else if input.parse::<Token!(|=)>().is_ok() {
            Ok(Self::BitOrAssign)
        } else if input.parse::<Token!(||)>().is_ok() {
            Ok(Self::Or)
        } else if input.parse::<Token!(|)>().is_ok() {
            Ok(Self::BitOr)
        } else if input.parse::<Token!(~)>().is_ok() {
            Ok(Self::BitNot)
        } else if input.parse::<Token!(^=)>().is_ok() {
            Ok(Self::BitXorAssign)
        } else if input.parse::<Token!(^)>().is_ok() {
            Ok(Self::BitXor)
        } else if input.parse::<Token!(<<=)>().is_ok() {
            Ok(Self::LShiftAssign)
        } else if input.parse::<Token!(<<)>().is_ok() {
            Ok(Self::LShift)
        } else if input.parse::<Token!(>>=)>().is_ok() {
            Ok(Self::RShiftAssign)
        } else if input.parse::<Token!(>>)>().is_ok() {
            Ok(Self::RShift)
        } else if input.parse::<Token!(>=)>().is_ok() {
            Ok(Self::GtOrEq)
        } else if input.parse::<Token!(>)>().is_ok() {
            Ok(Self::Gt)
        } else if input.parse::<Token!(<=)>().is_ok() {
            if input.parse::<Token!(>)>().is_ok() {
                Ok(Self::ThreeWay)
            } else {
                Ok(Self::LtOrEq)
            }
        } else if input.parse::<Token!(<)>().is_ok() {
            Ok(Self::Lt)
        } else if input.peek(Paren) {
            let _content;
            parenthesized!(_content in input);
            Ok(Self::Call)
        } else if input.parse::<Token!(,)>().is_ok() {
            Ok(Self::Comma)
        } else {
            Ok(Self::Convert(input.parse::<LitStr>()?.value()))
        }
    }
}

impl AsRef<str> for Operator {
    fn as_ref(&self) -> &str {
        match self {
            Self::New => "new",
            Self::Delete => "delete",
            Self::NewArray => "new []",
            Self::DeleteArray => "delete []",
            Self::Inc => "++",
            Self::Dec => "--",
            Self::Assign => "=",
            Self::AddAssign => "+=",
            Self::SubAssign => "-=",
            Self::MulAssign => "*=",
            Self::DivAssign => "/=",
            Self::ModAssign => "%=",
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Mod => "%",
            Self::Eq => "==",
            Self::NotEq => "!=",
            Self::Gt => ">",
            Self::GtOrEq => ">=",
            Self::Lt => "<",
            Self::LtOrEq => "<=",
            Self::ThreeWay => "<=>",
            Self::Not => "!",
            Self::And => "&&",
            Self::Or => "||",
            Self::BitNot => "~",
            Self::BitAnd => "&",
            Self::BitOr => "|",
            Self::BitXor => "^",
            Self::LShift => "<<",
            Self::RShift => ">>",
            Self::BitAndAssign => "&=",
            Self::BitOrAssign => "|=",
            Self::BitXorAssign => "^=",
            Self::LShiftAssign => "<<=",
            Self::RShiftAssign => ">>=",
            Self::Index => "[]",
            Self::Deref => "->",
            Self::DerefMember => "->*",
            Self::Call => "()",
            Self::Comma => ",",
            Self::Convert(v) => v.as_str(),
        }
    }
}
