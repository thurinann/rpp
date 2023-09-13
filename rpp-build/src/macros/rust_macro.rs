use std::borrow::Cow;

use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, ToTokens};
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::{bracketed, Block, ExprMacro, Ident, LitStr, Token};

use crate::ty::Type;
use crate::{Id, METADATA};

use super::ty::TyMacro;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
struct Arg {
    ident: Ident,
    cpp_ty: TyMacro,
    rust_ty: Option<syn::Type>,
    init: Option<String>,
}

impl Arg {
    pub fn replace(&self, to: impl AsRef<str>) -> Self {
        let to = to.as_ref();
        Self {
            ident: self.ident.clone(),
            cpp_ty: self.cpp_ty.replace("@Self@", to),
            rust_ty: self.rust_ty.clone(),
            init: self.init.as_ref().map(|x| x.replace("@Self@", to)),
        }
    }

    fn declaration(&self) -> String {
        format!("{} {}", self.cpp_ty, self.ident)
    }

    fn call_wrapper(&self) -> Cow<'_, str> {
        match &self.init {
            Some(init) => init.as_str().into(),
            _ => self.ident.to_string().into(),
        }
    }

    fn call_extern(&self) -> String {
        match METADATA.types.get(&self.cpp_ty.id()) {
            Some(Type::RRef(_)) => format!("std::move({})", self.ident),
            _ => self.ident.to_string(),
        }
    }

    fn check(&self) -> Option<TokenStream> {
        self.rust_ty.as_ref().map(|x| {
            let cpp = &self.cpp_ty;
            quote!(#[cfg(debug_assertions)] rpp::test_repr::<#cpp, #x>();)
        })
    }

    fn shadow(&self) -> Option<TokenStream> {
        METADATA
            .types
            .get(&self.cpp_ty.id())
            .filter(|x| x.is_rref())
            .map(|_| {
                let ident = &self.ident;
                quote!(
                    let mut slot = rpp::Slot::default();
                    let mut #ident = rpp::Move::move_to_slot(#ident, &mut slot);
                )
            })
    }
}

impl Parse for Arg {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = input.parse()?;
        input.parse::<Token!(:)>()?;
        let cpp_ty = input.parse()?;
        let rust_ty = match input.parse::<Token!(as)>() {
            Ok(_) => Some(input.parse()?),
            _ => None,
        };
        let init = match input.parse::<Token!(=)>() {
            Ok(_) => Some(input.parse::<LitStr>()?.value()),
            _ => Default::default(),
        };
        Ok(Self {
            ident,
            cpp_ty,
            rust_ty,
            init,
        })
    }
}

impl ToTokens for Arg {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ident = &self.ident;
        // let ty = &self.cpp_ty;
        let ty = self
            .rust_ty
            .as_ref()
            .map(|x| x.to_token_stream())
            .unwrap_or_else(|| self.cpp_ty.to_token_stream());
        quote!(mut #ident: #ty).to_tokens(tokens);
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct RustMacro {
    args: Vec<Arg>,
    cpp_ret: TyMacro,
    rust_ret: Option<syn::Type>,
    slot: Option<Ident>,
    body: Block,
}

impl RustMacro {
    pub fn replace(&self, to: impl AsRef<str>) -> Self {
        let to = to.as_ref();
        Self {
            args: self.args.iter().map(|x| x.replace(to)).collect(),
            cpp_ret: self.cpp_ret.replace("@Self@", to),
            rust_ret: self.rust_ret.clone(),
            slot: self.slot.clone(),
            body: self.body.clone(),
        }
    }

    pub fn collect_from_str(s: &str) -> Vec<(Self, usize, usize)> {
        let mut result = Vec::new();
        for (index, _) in s.match_indices("rust!(") {
            let mut count = 0;
            for (i, c) in s[index..].chars().enumerate() {
                if c == '(' {
                    count += 1;
                } else if c == ')' {
                    count -= 1;
                    if count == 0 {
                        if let Ok(m) = syn::parse_str::<ExprMacro>(&s[index..=index + i]) {
                            if let Ok(m) = syn::parse2(m.mac.tokens) {
                                result.push((m, index, index + i));
                            }
                        }
                        break;
                    }
                }
            }
        }
        result
    }

    pub fn declarations(&self) -> impl Iterator<Item = String> + '_ {
        self.args
            .iter()
            .map(|x| x.cpp_ty.declaration())
            .chain(std::iter::once(self.cpp_ret.declaration()))
    }

    pub fn call_wrapper(&self, pid: u64) -> String {
        let args = self
            .args
            .iter()
            .map(|x| x.call_wrapper())
            .collect::<Vec<_>>()
            .join(", ");
        let id = self.id();
        format!("___rpp_rust_wrapper_{pid}_{id}({args})")
    }

    pub fn wrapper_body(&self, pid: u64) -> String {
        let id = self.id();
        let name = format!("___rpp_rust_{pid}_{id}");
        let mut call = self
            .args
            .iter()
            .map(|x| x.call_extern())
            .collect::<Vec<_>>()
            .join(", ");
        let ret = &self.cpp_ret;
        if ret.is_void() {
            format!("{name}({call});")
        } else if ret.is_trivially_copyable() {
            format!("return {name}({call});")
        } else {
            if !call.is_empty() {
                call += ", ";
            }
            format!(
                r#"
    alignas({ret}) std::byte ___result[sizeof({ret})];
    {name}({call}reinterpret_cast<{ret}*>(___result));
    return std::move(*reinterpret_cast<{ret}*>(___result));
"#,
            )
        }
    }

    pub fn wrapper(&self, suffix: &str, pid: u64) -> impl Iterator<Item = String> + '_ {
        let mut args = self
            .args
            .iter()
            .map(|x| x.declaration())
            .collect::<Vec<_>>()
            .join(", ");
        let body = self.wrapper_body(pid);
        let id = self.id();
        let ret = &self.cpp_ret;
        let wrapper = format!("{ret} ___rpp_rust_wrapper_{pid}_{id}({args}) {{ {body} }}");
        let ret = if self.cpp_ret.is_void() {
            Cow::from("void")
        } else if !self.cpp_ret.is_trivially_copyable() {
            if !args.is_empty() {
                args += ", ";
            }
            args += "void *";
            Cow::from("void")
        } else {
            Cow::from(self.cpp_ret.to_string())
        };
        [
            format!(r#"extern "C" {ret} ___rpp_rust_{pid}_{id}({args}){suffix};"#),
            wrapper,
        ]
        .into_iter()
    }

    fn check_ret(&self) -> Option<TokenStream> {
        self.rust_ret.as_ref().map(|x| {
            let cpp_ret = &self.cpp_ret;
            quote!(#[cfg(debug_assertions)] rpp::test_repr::<#cpp_ret, #x>();)
        })
    }

    fn check_errors(&self) -> Option<TokenStream> {
        if let Some(Arg { cpp_ty, .. }) =
            self.args.iter().find(|x| !x.cpp_ty.is_trivially_copyable())
        {
            let msg = format!("`{cpp_ty}` is not trivially copyable, use &&");
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

    pub fn to_tokens(&self, pid: u64, tokens: &mut TokenStream) {
        if let Some(errors) = self.check_errors() {
            errors
        } else {
            let args = &self.args;
            let ident = format_ident!("___rpp_rust_{pid}_{}", self.id());
            let check_args = self.args.iter().map(|x| x.check());
            let check_ret = self.check_ret();
            let shadow = self.args.iter().map(|x| x.shadow());
            let prepare = quote!(#(#check_args)* #check_ret #(#shadow)*);
            let body = &self.body;
            if self.cpp_ret.is_void() {
                quote!(#[no_mangle] extern "C" fn #ident(#(#args,)*) { #prepare #body })
            } else {
                let ret = self
                    .rust_ret
                    .as_ref()
                    .map_or_else(|| self.cpp_ret.to_token_stream(), |x| x.to_token_stream());
                if self.cpp_ret.is_trivially_copyable() {
                    quote!(#[no_mangle] extern "C" fn #ident(#(#args,)*) -> #ret { #prepare #body })
                } else if self.cpp_ret.is_trivially_relocatable() {
                    quote!(
                        #[no_mangle]
                        extern "C" fn #ident(#(#args,)* ___res: *mut ::core::ffi::c_void) {
                            #prepare
                            unsafe { ::core::ptr::write(___res as _, #body); }
                        }
                    )
                } else {
                    let slot = &self.slot;
                    quote!(
                        #[no_mangle]
                        extern "C" fn #ident(#(#args,)* ___res: *mut ::core::ffi::c_void) {
                            #prepare
                            let mut #slot = Slot::default();
                            unsafe { rpp::Move::move_to_ptr(#body, ___res); }
                        }
                    )
                }
            }
        }
        .to_tokens(tokens);
    }
}

impl Parse for RustMacro {
    fn parse(input: ParseStream) -> Result<Self> {
        let content;
        bracketed!(content in input);
        let args = Punctuated::<_, Token!(,)>::parse_terminated(&content)?
            .into_iter()
            .collect();
        let (rust_ret, cpp_ret) = match input.parse::<Token!(->)>() {
            Ok(_) if input.peek(LitStr) => (None, input.parse()?),
            Ok(_) => (Some(input.parse()?), {
                input.parse::<Token!(as)>()?;
                input.parse()?
            }),
            _ => Default::default(),
        };
        let slot = match input.parse::<Token!(=>)>() {
            Ok(_) => Some(input.parse()?),
            _ => None,
        };
        let body = input.parse()?;
        Ok(Self {
            args,
            cpp_ret,
            rust_ret,
            slot,
            body,
        })
    }
}
