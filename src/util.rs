use darling::Error;
use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use syn::punctuated::Punctuated;
use syn::{
    Attribute, Expr, ExprLit, GenericArgument, GenericParam, Lit, LitStr, Meta, Pat, PathArguments,
    ReturnType, Token, Type, TypePath, WhereClause,
};

use crate::model::FieldTransform;

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

pub fn finish_as_error<T>(acc: darling::error::Accumulator) -> darling::Result<T> {
    Err(Error::multiple(acc.into_inner()))
}

pub fn get_doc(attrs: &[Attribute]) -> impl Iterator<Item = &Expr> {
    attrs
        .iter()
        .filter_map(|a| match &a.meta {
            Meta::NameValue(meta) => Some(meta),
            _ => None,
        })
        .filter(|m| m.path.is_ident("doc"))
        .map(|m| &m.value)
}

fn first_punct<T, P>(p: &Punctuated<T, P>) -> Option<&T> {
    p.pairs().next().map(|p| p.into_value())
}

fn last_punct<T, P>(p: &Punctuated<T, P>) -> Option<&T> {
    p.pairs().next_back().map(|p| p.into_value())
}

pub fn first_generic_arg(mut ty: &Type) -> Option<&Type> {
    fn inner(ty: &TypePath) -> Option<&Type> {
        let seg = last_punct(&ty.path.segments)?;
        if let PathArguments::AngleBracketed(a) = &seg.arguments {
            if let GenericArgument::Type(ty) = first_punct(&a.args)? {
                return Some(ty);
            }
        }
        None
    }

    loop {
        match ty {
            Type::Group(t) => ty = &t.elem,
            Type::Paren(t) => ty = &t.elem,
            Type::Path(t) => break inner(t),
            _ => break None,
        }
    }
}

pub fn empty_where_clause() -> WhereClause {
    WhereClause {
        where_token: <Token![where]>::default(),
        predicates: Punctuated::new(),
    }
}

pub fn lit_str_expr(lit: &str) -> Expr {
    Expr::Lit(ExprLit {
        lit: Lit::Str(LitStr::new(lit, Span::call_site())),
        attrs: Vec::new(),
    })
}

pub fn unwrap_expr(mut expr: Expr) -> Expr {
    loop {
        match expr {
            Expr::Group(g) => expr = *g.expr,
            Expr::Paren(g) => expr = *g.expr,
            o => break o,
        }
    }
}

pub fn to_field_transform(value: Expr, acc: &mut darling::error::Accumulator) -> FieldTransform {
    // using `_` as the type in error cases leads to less rustc follow-up errors
    // from type mismatches/incorrect types/unreachable code than using Infallible
    // or ! or basically anything else. just one error that it's invalid.
    let value = unwrap_expr(value);
    let Expr::Closure(value) = value else {
        let transform = FieldTransform {
            lifetimes: TokenStream::new(),
            inputs: vec![syn::parse_quote!(invalid_setter: _)],
            body: syn::parse_quote!(invalid_setter),
        };

        acc.push(Error::custom("expected closure").with_span(&value));
        return transform;
    };

    for attr in &value.attrs {
        let err = Error::custom("closure must not have attributes");
        acc.push(err.with_span(attr));
    }

    if let Some(constness) = &value.constness {
        let err = Error::custom("closure must not have constness, it is implicitly const here");
        acc.push(err.with_span(constness));
    }

    if let Some(movability) = &value.movability {
        let err = Error::custom("closure must not be static");
        acc.push(err.with_span(movability));
    }

    if let Some(asyncness) = &value.asyncness {
        let err = Error::custom("closure must not be async");
        acc.push(err.with_span(asyncness));
    }

    if let Some(capture) = &value.capture {
        let err = Error::custom("closure cannot have captures");
        acc.push(err.with_span(capture));
    }

    if !matches!(value.output, ReturnType::Default) {
        let err = Error::custom("closure must not have an explicit return type");
        acc.push(err.with_span(&value.output));
    }

    let inputs = value
        .inputs
        .into_iter()
        .map(|pat| match pat {
            Pat::Type(pat_type) => pat_type,
            pat => {
                let err = Error::custom("closure inputs must all have an explicit type");
                acc.push(err.with_span(&pat));
                syn::parse_quote!(#pat: _)
            },
        })
        .collect();

    let lifetimes = value
        .lifetimes
        .map(|l| {
            let l = l.lifetimes;
            quote::quote! { < #l > }
        })
        .unwrap_or_default();

    FieldTransform {
        lifetimes,
        inputs,
        body: value.body,
    }
}
