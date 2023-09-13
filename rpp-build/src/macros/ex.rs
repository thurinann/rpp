use std::fmt;
use std::hash::Hash;

use proc_macro2::{Span, TokenStream};
use quote::{quote, ToTokens};
use syn::parse::Parse;
use syn::punctuated::Punctuated;
use syn::{bracketed, Expr, Ident, LitStr, Token};

use crate::ty::Type;
use crate::{Id, METADATA};

use super::ty::TyMacro;

#[derive(Hash, Debug)]
struct Arg {
    is_unsafe: bool,
    ident: Ident,
    cpp_ty: TyMacro,
    rust_ty: Option<syn::Type>,
    init: Option<Expr>,
}

impl Arg {
    fn cpp_call(&self) -> String {
        if self.cpp_ty.is_rref() {
            format!("std::move({})", self.ident)
        } else {
            self.ident.to_string()
        }
    }

    fn rust_call(&self) -> TokenStream {
        let ident = &self.ident;
        if let Some(Type::RRef(ty)) = METADATA.types.get(&self.cpp_ty.id()) {
            if ty.is_trivially_relocatable() {
                quote!(unsafe { ::core::mem::transmute(::core::pin::Pin::new(&mut #ident)) })
            } else {
                quote!(unsafe { ::core::mem::transmute(#ident.as_mut()) })
            }
        } else {
            quote!(#ident)
        }
    }

    fn check(&self) -> Option<TokenStream> {
        self.rust_ty.as_ref().map(|x| {
            let cpp = &self.cpp_ty;
            quote!(#[cfg(debug_assertions)] rpp::test_repr::<#cpp, #x>();)
        })
    }

    fn shadow(&self) -> TokenStream {
        let ident = &self.ident;
        let value = match &self.init {
            Some(init) => quote!({#init}),
            _ => quote!(#ident),
        };
        quote!(let mut #ident = #value;)
    }
}

impl Parse for Arg {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let is_unsafe = input.parse::<Token!(unsafe)>().is_ok();
        let ident = input.parse()?;
        input.parse::<Token!(:)>()?;
        let (rust_ty, cpp_ty) = if input.peek(LitStr) {
            (None, input.parse()?)
        } else {
            (Some(input.parse()?), {
                input.parse::<Token!(as)>()?;
                input.parse()?
            })
        };
        let init = match input.parse::<Token!(=)>() {
            Ok(_) => Some(input.parse()?),
            _ => None,
        };
        Ok(Self {
            is_unsafe,
            ident,
            cpp_ty,
            rust_ty,
            init,
        })
    }
}

impl fmt::Display for Arg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.cpp_ty, self.ident)
    }
}

impl ToTokens for Arg {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.ident;
        if let Some(Type::RRef(ty)) = METADATA.types.get(&self.cpp_ty.id()) {
            quote!(#ident: ::core::pin::Pin<&mut #ty>)
        } else {
            let ty = self
                .rust_ty
                .as_ref()
                .map(|x| x.to_token_stream())
                .unwrap_or_else(|| self.cpp_ty.to_token_stream());
            quote!(#ident: #ty)
        }
        .to_tokens(tokens);
    }
}

#[derive(Hash, Debug)]
pub struct ExMacro {
    args: Vec<Arg>,
    body: String,
    slot: Option<Ident>,
    cpp_ret: TyMacro,
    rust_ret: Option<syn::Type>,
}

impl ExMacro {
    pub fn declarations(&self) -> impl Iterator<Item = String> + '_ {
        self.args
            .iter()
            .map(|x| x.cpp_ty.declaration())
            .chain(std::iter::once(self.cpp_ret.declaration()))
    }

    fn check_ret(&self) -> Option<TokenStream> {
        self.rust_ret.as_ref().map(|x| {
            let cpp_ret = &self.cpp_ret;
            quote!(#[cfg(debug_assertions)] rpp::test_repr::<#cpp_ret, #x>();)
        })
    }

    fn extern_decl(&self) -> TokenStream {
        let args = &self.args;
        if self.cpp_ret.is_void() {
            quote!(__fun(#(#args,)*))
        } else if !self.cpp_ret.is_trivially_copyable() {
            quote!(__fun(#(#args,)* __res: *mut ::core::ffi::c_void))
        } else {
            let ret = self
                .rust_ret
                .as_ref()
                .map_or_else(|| self.cpp_ret.to_token_stream(), |x| x.to_token_stream());
            quote!(__fun(#(#args,)*) -> #ret)
        }
    }

    fn extern_call(&self) -> TokenStream {
        let args = self.args.iter().map(|x| x.rust_call());
        if self.cpp_ret.is_void() {
            quote!(__fun(#(#args,)*);)
        } else if !self.cpp_ret.is_trivially_relocatable() {
            let slot = &self.slot;
            quote!(
                __fun(#(#args,)* #slot.as_mut_ptr() as _);
                rpp::StackBox::pin(#slot)
            )
        } else if !self.cpp_ret.is_trivially_copyable() {
            quote!(
                let mut __res = ::core::mem::MaybeUninit::uninit();
                __fun(#(#args,)* __res.as_mut_ptr() as _);
                __res.assume_init()
            )
        } else {
            quote!(__fun(#(#args,)*))
        }
    }

    fn check_errors(&self) -> Option<TokenStream> {
        if let Some(Arg { cpp_ty, .. }) = self
            .args
            .iter()
            .find(|x| x.cpp_ty.is_pointer() && !x.is_unsafe)
        {
            let msg = format!("`{cpp_ty}` is pointer and must be marked as unsafe");
            let lit = LitStr::new(&msg, Span::call_site());
            Some(quote!(compile_error!(#lit)))
        } else if let Some(Arg { cpp_ty, .. }) =
            self.args.iter().find(|x| !x.cpp_ty.is_trivially_copyable())
        {
            let msg = format!("`{cpp_ty}` is non trivially copyable, use &&");
            let lit = LitStr::new(&msg, Span::call_site());
            Some(quote!(compile_error!(#lit)))
        } else if self.slot.is_none() && !self.cpp_ret.is_trivially_relocatable() {
            let msg = format!("`{}` is not trivially relocatable, use slot", self.cpp_ret);
            let lit = LitStr::new(&msg, Span::call_site());
            Some(quote!(compile_error!(#lit)))
        } else {
            None
        }
    }
}

impl Parse for ExMacro {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;
        bracketed!(content in input);
        let args = Punctuated::<_, Token!(,)>::parse_terminated(&content)?
            .into_iter()
            .collect();
        let cpp_ret = match input.parse::<Token!(->)>() {
            Ok(_) => input.parse()?,
            _ => Default::default(),
        };
        let rust_ret = match input.parse::<Token!(as)>() {
            Ok(_) => Some(input.parse::<syn::Type>()?),
            _ => Default::default(),
        };
        input.parse::<Token!(:)>()?;
        let body = input.parse::<LitStr>()?.value();
        let slot = match input.parse::<Token!(=>)>() {
            Ok(_) => Some(input.parse()?),
            _ => None,
        };
        Ok(Self {
            args,
            slot,
            body,
            cpp_ret,
            rust_ret,
        })
    }
}

impl fmt::Display for ExMacro {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let id = self.id();
        let body = &self.body;
        let ident = format!("___rpp_impl_{id}");
        let mut args = self
            .args
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        let ret = &self.cpp_ret;
        writeln!(f, "inline {ret} {ident}({args}) {{ {body} }}")?;
        let calls = self
            .args
            .iter()
            .map(|x| x.cpp_call())
            .collect::<Vec<_>>()
            .join(", ");
        let call = format!("{ident}({calls})");
        let ident = format!("___rpp_{id}");
        if ret.is_void() {
            format!(r#"extern "C" void {ident}({args}) {{ {call}; }}"#).fmt(f)
        } else if ret.is_trivially_copyable() {
            format!(r#"extern "C" {ret} {ident}({args}) {{ return {call}; }}"#).fmt(f)
        } else {
            if !args.is_empty() {
                args += ", ";
            }
            format!(r#"extern "C" void {ident}({args}void * ___rpp_res) {{ ::new (___rpp_res) ({ret}) ({call}); }}"#).fmt(f)
        }
    }
}

impl ToTokens for ExMacro {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        if let Some(errors) = self.check_errors() {
            errors
        } else {
            let decl = self.extern_decl();
            let call = self.extern_call();
            let link = LitStr::new(&format!("___rpp_{}", self.id()), Span::call_site());
            let check_args = self.args.iter().map(|x| x.check());
            let check_ret = self.check_ret();
            let shadow = self.args.iter().map(|x| x.shadow());
            quote!({
                extern "C" { #[link_name = #link] #[allow(improper_ctypes)] fn #decl; }
                #(#check_args)*
                #check_ret
                #(#shadow)*
                unsafe { #call }
            })
        }
        .to_tokens(tokens);
    }
}
