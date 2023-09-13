use std::hash::Hash;

use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::parse::{Parse, ParseStream, Result};
use syn::{ConstParam, GenericParam, Generics, Ident, TypeParam, Visibility};

#[derive(Hash, Debug)]
pub struct WrapTemplateMacro {
    visibility: Visibility,
    ident: Ident,
    generics: Vec<GenericParam>,
}

impl Parse for WrapTemplateMacro {
    fn parse(input: ParseStream) -> Result<Self> {
        let visibility = input.parse()?;
        let ident = input.parse()?;
        let generics = input.parse::<Generics>()?.params.into_iter().collect();
        Ok(Self {
            visibility,
            ident,
            generics,
        })
    }
}

impl ToTokens for WrapTemplateMacro {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let vis = &self.visibility;
        let ident = &self.ident;
        let bind = format_ident!("T{ident}Binding");
        let generics = &self.generics;
        let mut type_bounds = Vec::new();
        let mut tuple = Vec::new();
        let mut idents = Vec::new();
        for g in generics {
            match g {
                GenericParam::Type(TypeParam { ident, .. }) => {
                    type_bounds.push(quote!(#ident: rpp::CppRepr,));
                    tuple.push(quote!(<#ident as rpp::CppRepr>::Repr));
                    idents.push(ident.to_token_stream());
                }
                GenericParam::Const(ConstParam {
                    ident,
                    ty: syn::Type::Path(p),
                    ..
                }) => {
                    idents.push(ident.to_token_stream());
                    let ty = p
                        .path
                        .get_ident()
                        .map(|x| x.to_string())
                        .unwrap_or_default();
                    match ty.as_str() {
                        "isize" => tuple.push(quote!(rpp::SParam<#ident>)),
                        "usize" => tuple.push(quote!(rpp::UParam<#ident>)),
                        "bool" => tuple.push(quote!(rpp::BParam<#ident>)),
                        "char" => tuple.push(quote!(rpp::CParam<#ident>)),
                        _ => (),
                    }
                }
                _ => (),
            }
        }
        quote!(
            #[repr(transparent)]
            #vis struct #ident<#(#generics),*>(<(#(#tuple),*) as #bind>::Type)
            where
                #(#type_bounds)*
                (#(#tuple),*): #bind;


            #vis trait #bind { type Type; }

            unsafe impl<#(#generics),*> rpp::CppRepr for #ident<#(#idents),*>
            where
                #(#type_bounds)*
                (#(#tuple),*): #bind,
            {
                type Repr = <(#(#tuple),*) as #bind>::Type;
            }
        )
        .to_tokens(tokens);
    }
}

// #[repr(transparent)]
// pub struct UniquePtr<T>(<<T as CppRepr>::Repr as TUniquePtrBinding>::Type)
// where
//     T: CppRepr,
//     <T as CppRepr>::Repr: TUniquePtrBinding;

// unsafe impl<T> CppRepr for UniquePtr<T>
// where
//     T: CppRepr,
//     <T as CppRepr>::Repr: TUniquePtrBinding,
// {
//     type Repr = <<T as CppRepr>::Repr as TUniquePtrBinding>::Type;
// }

// pub trait TUniquePtrBinding {
//     type Type;
// }

// #[repr(transparent)]
// pub struct QGenericMatrix<const N: usize, const M: usize, T>(
//     <(UParam<N>, UParam<M>, <T as CppRepr>::Repr) as TQGenericMatrixBinding>::Type,
// )
// where
//     T: CppRepr,
//     (UParam<N>, UParam<M>, <T as CppRepr>::Repr): TQGenericMatrixBinding;

// unsafe impl<const N: usize, const M: usize, T> CppRepr for QGenericMatrix<N, M, T>
// where
//     T: CppRepr,
//     (UParam<N>, UParam<M>, <T as CppRepr>::Repr): TQGenericMatrixBinding,
// {
//     type Repr = <(UParam<N>, UParam<M>, <T as CppRepr>::Repr) as TQGenericMatrixBinding>::Type;
// }

// pub trait TQGenericMatrixBinding {
//     type Type;
// }
