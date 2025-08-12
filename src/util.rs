use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use syn::punctuated::Punctuated;
use syn::{
    Attribute, Expr, ExprLit, GenericArgument, GenericParam, Lit, LitStr, Meta, Pat, PathArguments,
    ReturnType, Token, Type, WhereClause,
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

pub fn push_error(res: &mut syn::Result<()>, new: syn::Error) {
    match res {
        Ok(()) => *res = Err(new),
        Err(err) => err.combine(new),
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

pub fn unwrap_expr(expr: Expr) -> Expr {
    match expr {
        Expr::Group(expr) => unwrap_expr(*expr.expr),
        Expr::Paren(expr) => unwrap_expr(*expr.expr),
        expr => expr,
    }
}

pub fn to_field_transform(value: Expr) -> (FieldTransform, syn::Result<()>) {
    let value = unwrap_expr(value);
    let Expr::Closure(value) = value else {
        return (
            FieldTransform {
                lifetimes: TokenStream::new(),
                inputs: vec![syn::parse_quote!(invalid_setter: ::core::convert::Infallible)],
                body: syn::parse_quote!(match invalid_setter {}),
            },
            Err(syn::Error::new_spanned(value, "expected closure")),
        );
    };

    let mut errors = Ok(());

    for attr in &value.attrs {
        push_error(
            &mut errors,
            syn::Error::new_spanned(attr, "closure must not have attributes"),
        );
    }

    if let Some(constness) = value.constness {
        push_error(
            &mut errors,
            syn::Error::new_spanned(
                constness,
                "closure must not have constness, it is implicitly const here",
            ),
        );
    }

    if let Some(movability) = value.movability {
        push_error(
            &mut errors,
            syn::Error::new_spanned(movability, "closure must not be static"),
        );
    }

    if let Some(asyncness) = value.asyncness {
        push_error(
            &mut errors,
            syn::Error::new_spanned(asyncness, "closure must not be async"),
        );
    }

    if let Some(capture) = value.capture {
        push_error(
            &mut errors,
            syn::Error::new_spanned(capture, "closure cannot have captures"),
        );
    }

    if !matches!(value.output, ReturnType::Default) {
        push_error(
            &mut errors,
            syn::Error::new_spanned(
                value.output,
                "closure must not have an explicit return type",
            ),
        );
    }

    let inputs = value
        .inputs
        .into_iter()
        .map(|pat| match pat {
            Pat::Type(pat_type) => pat_type,
            pat => {
                push_error(
                    &mut errors,
                    syn::Error::new_spanned(&pat, "closure inputs must all have an explicit type"),
                );
                syn::parse_quote!(#pat: ::core::convert::Infallible)
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

    (
        FieldTransform {
            lifetimes,
            inputs,
            body: value.body,
        },
        errors,
    )
}
