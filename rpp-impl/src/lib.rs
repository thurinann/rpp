use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use rpp_build::macros::class::ClassMacro;
use rpp_build::macros::enum_val::EnumValMacro;
use rpp_build::macros::expand::ExpandMacro;
use rpp_build::macros::export::ExportMacro;
use rpp_build::macros::par_ty::ParTyMacro;
use rpp_build::macros::ty::TyMacro;
use rpp_build::macros::val::ValMacro;
use rpp_build::macros::wrap_const::WrapConstMacro;
use rpp_build::macros::wrap_enum::WrapEnumMacro;
use rpp_build::macros::wrap_struct::WrapStructMacro;
use rpp_build::macros::wrap_template::WrapTemplateMacro;
use rpp_build::macros::{ex::ExMacro, var::VarMacro};
use rpp_build::METADATA;
use syn::parse_macro_input;

#[proc_macro]
pub fn __init_metadata(_input: TokenStream) -> TokenStream {
    METADATA.to_token_stream().into()
}

#[proc_macro]
pub fn init(input: TokenStream) -> TokenStream {
    input
}

#[proc_macro]
pub fn ty(input: TokenStream) -> TokenStream {
    let m = parse_macro_input!(input as TyMacro);
    m.into_token_stream().into()
}

#[proc_macro]
pub fn par_ty(input: TokenStream) -> TokenStream {
    let m = parse_macro_input!(input as ParTyMacro);
    m.into_token_stream().into()
}

#[proc_macro]
pub fn var(input: TokenStream) -> TokenStream {
    let m = parse_macro_input!(input as VarMacro);
    m.into_token_stream().into()
}

#[proc_macro]
pub fn val(input: TokenStream) -> TokenStream {
    let m = parse_macro_input!(input as ValMacro);
    m.into_token_stream().into()
}

#[proc_macro]
pub fn enum_val(input: TokenStream) -> TokenStream {
    let m = parse_macro_input!(input as EnumValMacro);
    m.into_token_stream().into()
}

#[proc_macro]
pub fn wrap_const(input: TokenStream) -> TokenStream {
    let m = parse_macro_input!(input as WrapConstMacro);
    m.into_token_stream().into()
}

#[proc_macro]
pub fn wrap_struct(input: TokenStream) -> TokenStream {
    let m = parse_macro_input!(input as WrapStructMacro);
    m.into_token_stream().into()
}

#[proc_macro]
pub fn wrap_template(input: TokenStream) -> TokenStream {
    let m = parse_macro_input!(input as WrapTemplateMacro);
    m.into_token_stream().into()
}

#[proc_macro]
pub fn wrap_enum(input: TokenStream) -> TokenStream {
    let m = parse_macro_input!(input as WrapEnumMacro);
    m.into_token_stream().into()
}

#[proc_macro]
pub fn wrap_flags(input: TokenStream) -> TokenStream {
    let m = parse_macro_input!(input as WrapEnumMacro);
    let ident = m.ident();
    quote!(
        #m

        impl ::core::ops::BitOr for #ident {
            type Output = Self;

            fn bitor(self, rhs: Self) -> Self::Output {
                Self(self.0 | rhs.0)
            }
        }
        impl ::core::ops::BitOrAssign for #ident {
            fn bitor_assign(&mut self, rhs: Self) {
                self.0 |= rhs.0;
            }
        }
        impl ::core::ops::BitAnd for #ident {
            type Output = Self;

            fn bitand(self, rhs: Self) -> Self::Output {
                Self(self.0 & rhs.0)
            }
        }
        impl ::core::ops::BitAndAssign for #ident {
            fn bitand_assign(&mut self, rhs: Self) {
                self.0 &= rhs.0;
            }
        }
        impl ::core::ops::BitXor for #ident {
            type Output = Self;

            fn bitxor(self, rhs: Self) -> Self::Output {
                Self(self.0 ^ rhs.0)
            }
        }
        impl ::core::ops::BitXorAssign for #ident {
            fn bitxor_assign(&mut self, rhs: Self) {
                self.0 ^= rhs.0;
            }
        }
        impl ::core::ops::Sub for #ident {
            type Output = Self;

            fn sub(self, rhs: Self) -> Self {
                Self(self.0 - rhs.0)
            }
        }
        impl ::core::ops::SubAssign for #ident {
            fn sub_assign(&mut self, rhs: Self) {
                self.0 -= rhs.0;
            }
        }
        impl ::core::ops::Not for #ident {
            type Output = Self;

            fn not(self) -> Self::Output {
                Self(!self.0)
            }
        }
    )
    .into()
}

#[proc_macro]
pub fn ex(input: TokenStream) -> TokenStream {
    let m = parse_macro_input!(input as ExMacro);
    m.into_token_stream().into()
}

#[proc_macro]
pub fn expand(input: TokenStream) -> TokenStream {
    let m = parse_macro_input!(input as ExpandMacro);
    m.into_token_stream().into()
}

#[proc_macro]
pub fn export(input: TokenStream) -> TokenStream {
    let m = parse_macro_input!(input as ExportMacro);
    m.into_token_stream().into()
}

#[proc_macro]
pub fn class(input: TokenStream) -> TokenStream {
    let m = parse_macro_input!(input as ClassMacro);
    m.into_token_stream().into()
}
