use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::{GenericParam, Generics, Token};

#[derive(Debug, Clone, Copy)]
pub struct ImplGenerics<'a>(pub &'a Generics);

#[derive(Debug, Clone, Copy)]
pub struct TypeGenerics<'a>(pub &'a Generics);

impl ToTokens for ImplGenerics<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for generic in &self.0.params {
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
        for generic in &self.0.params {
            match generic {
                GenericParam::Lifetime(param) => param.lifetime.to_tokens(tokens),
                GenericParam::Type(param) => param.ident.to_tokens(tokens),
                GenericParam::Const(param) => param.ident.to_tokens(tokens),
            }

            <Token![,]>::default().to_tokens(tokens);
        }
    }
}
