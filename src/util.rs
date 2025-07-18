use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::punctuated::Punctuated;
use syn::{Attribute, Expr, GenericArgument, GenericParam, Meta, PathArguments, Token, Type};

#[derive(Debug, Clone, Copy)]
pub struct ImplGenerics<'a>(pub &'a Punctuated<GenericParam, Token![,]>);

#[derive(Debug, Clone, Copy)]
pub struct TypeGenerics<'a>(pub &'a Punctuated<GenericParam, Token![,]>);

impl ToTokens for ImplGenerics<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for generic in self.0 {
            match generic {
                GenericParam::Lifetime(param) => param.to_tokens(tokens),
                GenericParam::Type(param) => {
                    param.ident.to_tokens(tokens);
                    param.colon_token.to_tokens(tokens);
                    param.bounds.to_tokens(tokens);
                },
                GenericParam::Const(param) => {
                    param.const_token.to_tokens(tokens);
                    param.ident.to_tokens(tokens);
                    param.colon_token.to_tokens(tokens);
                    param.ty.to_tokens(tokens);
                },
            }

            <Token![,]>::default().to_tokens(tokens);
        }
    }
}

impl ToTokens for TypeGenerics<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for generic in self.0 {
            match generic {
                GenericParam::Lifetime(param) => param.lifetime.to_tokens(tokens),
                GenericParam::Type(param) => param.ident.to_tokens(tokens),
                GenericParam::Const(param) => param.ident.to_tokens(tokens),
            }

            <Token![,]>::default().to_tokens(tokens);
        }
    }
}

pub fn get_doc(attrs: &[Attribute]) -> Vec<Expr> {
    attrs
        .iter()
        .filter_map(|a| match &a.meta {
            Meta::NameValue(meta) => Some(meta),
            _ => None,
        })
        .filter(|m| m.path.is_ident("doc"))
        .map(|m| m.value.clone())
        .collect()
}

pub fn first_generic_arg(ty: &Type) -> &Type {
    fn inner(ty: &Type) -> Option<&Type> {
        match ty {
            Type::Path(path) => {
                let seg = path.path.segments.last()?;
                let PathArguments::AngleBracketed(a) = &seg.arguments else {
                    return None;
                };
                let Some(GenericArgument::Type(ty)) = a.args.first() else {
                    return None;
                };
                Some(ty)
            },
            Type::Paren(t) => inner(&t.elem),
            _ => None,
        }
    }

    inner(ty).unwrap_or(ty)
}
