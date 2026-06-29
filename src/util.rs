use darling::{Error, FromMeta};
use proc_macro2::{Span, TokenStream};
use quote::ToTokens;
use syn::punctuated::{Pair, Punctuated};
use syn::token::Bracket;
use syn::{
    AttrStyle, Attribute, BoundLifetimes, Expr, ExprLit, GenericArgument, GenericParam, Ident, Lit,
    LitBool, LitStr, Meta, MetaNameValue, Pat, PathArguments, ReturnType, Token, Type, TypePath,
    WhereClause,
};

use crate::model::FieldTransform;

/// To be inserted in the generic parameter list for `impl<>` or a function.
///
/// Similar to [`syn::ImplGenerics`] but appends a comma unconditionally.
#[derive(Debug, Clone, Copy)]
pub struct ImplGenerics<'a>(pub &'a Punctuated<GenericParam, Token![,]>);

/// To be inserted in the generic argument list for the type of an `impl` block
/// of a matching [`ImplGenerics`].
///
/// Similar to [`syn::TypeGenerics`] but appends a comma unconditionally.
#[derive(Debug, Clone, Copy)]
pub struct TypeGenerics<'a>(pub &'a Punctuated<GenericParam, Token![,]>);

/// To be inserted into the generic parameter list for a type definition.
///
/// Similar to `Punctuated<GenericParam, Token![,]>` but appends a comma
/// unconditionally.
#[derive(Debug, Clone, Copy)]
pub struct StructGenerics<'a>(pub &'a Punctuated<GenericParam, Token![,]>);

impl ToTokens for ImplGenerics<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for generic in self.0.pairs() {
            match generic.into_value() {
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
        for generic in self.0.pairs() {
            match generic.into_value() {
                GenericParam::Lifetime(param) => param.lifetime.to_tokens(tokens),
                GenericParam::Type(param) => param.ident.to_tokens(tokens),
                GenericParam::Const(param) => param.ident.to_tokens(tokens),
            }

            <Token![,]>::default().to_tokens(tokens);
        }
    }
}

impl ToTokens for StructGenerics<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for generic in self.0.pairs() {
            generic.into_value().to_tokens(tokens);
            <Token![,]>::default().to_tokens(tokens);
        }
    }
}

/// Represents a generic parameter list without where-clause.
#[derive(Debug)]
pub struct AngleBracketedGenerics {
    lt_token: Token![<],
    params: Punctuated<GenericParam, Token![,]>,
    gt_token: Token![>],
}

impl ToTokens for AngleBracketedGenerics {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.lt_token.to_tokens(tokens);
        self.params.to_tokens(tokens);
        self.gt_token.to_tokens(tokens);
    }
}

impl From<BoundLifetimes> for AngleBracketedGenerics {
    fn from(value: BoundLifetimes) -> Self {
        Self {
            lt_token: value.lt_token,
            params: value.lifetimes,
            gt_token: value.gt_token,
        }
    }
}

/// [`FromMeta`] value that accepts a [`bool`], falling back to `T`.
#[derive(Debug)]
pub enum BoolOr<T> {
    Bool(bool),
    Value(T),
}

impl<T: FromMeta> FromMeta for BoolOr<T> {
    fn from_expr(expr: &Expr) -> darling::Result<Self> {
        if let Expr::Lit(ExprLit {
            lit: Lit::Bool(LitBool { value, .. }),
            ..
        }) = *expr
        {
            return Ok(BoolOr::Bool(value));
        }

        T::from_expr(expr).map(BoolOr::Value)
    }
}

/// Allows matching on any meta-item, no matter whether word, value, or list.
///
/// This doesn't further parse or store any information about the meta-item. It
/// is simply used when you expect an item.
///
/// Wrapped in [`Option`] it can also be used to check for the presence of a
/// field without making the parser check its contents.
#[derive(Default, Debug)]
pub struct AnyItem(());

impl FromMeta for AnyItem {
    fn from_meta(_item: &Meta) -> darling::Result<Self> {
        // in case we need the span in the future, grab it from the item here.
        Ok(Self(()))
    }
}

/// Writes the accumulated errors into a [`TokenStream`].
///
/// Panics if the accumulator is empty. This must only be called if there
/// actually are errors in the accumulator.
pub fn into_write_errors(acc: darling::error::Accumulator) -> TokenStream {
    Error::multiple(acc.into_inner()).write_errors()
}

/// Iterates over all the meta items with the path `doc` in attributes.
pub fn iter_doc_attrs(attrs: &[Attribute]) -> impl Iterator<Item = &Attribute> {
    attrs.iter().filter(|f| f.path().is_ident("doc"))
}

/// Creates an attribute corresponding to `#[doc = $lit]`.
pub fn doc_str_attr(lit: &str) -> Attribute {
    Attribute {
        pound_token: <Token![#]>::default(),
        style: AttrStyle::Outer,
        bracket_token: Bracket::default(),
        meta: Meta::NameValue(MetaNameValue {
            path: simple_ident("doc").into(),
            eq_token: <Token![=]>::default(),
            value: lit_str_expr(lit),
        }),
    }
}

/// Finds the `deprecated` attribute.
pub fn find_deprecated(attrs: &[Attribute]) -> Option<&Attribute> {
    attrs.iter().find(|a| a.path().is_ident("deprecated"))
}

/// Returns a deprecation suppression if the attribute is `Some`.
pub fn allow_deprecated(attr: Option<&Attribute>) -> Option<TokenStream> {
    // odd sig just so i don't accidentally pass the wrong thing
    attr.map(|_| quote::quote! { #[allow(deprecated)] })
}

/// Returns a simple, non-raw ident.
pub fn simple_ident(ident: &str) -> Ident {
    Ident::new(ident, Span::call_site())
}

fn first_punct<T, P>(p: &Punctuated<T, P>) -> Option<&T> {
    p.pairs().next().map(|p| p.into_value())
}

fn last_punct<T, P>(p: &Punctuated<T, P>) -> Option<&T> {
    p.pairs().next_back().map(|p| p.into_value())
}

/// Gets the first generic argument, if the last path segment of the type has
/// any. Generic arguments anywhere else in the path are ignored.
///
/// [`Type::Group`] and [`Type::Paren`] check their inner type instead. If the
/// final unwrapped value isn't [`Type::Path`], returns [`None`].
///
/// **Examples:**
/// - `Option<i32>` -> `Some(i32)`
/// - `Result<i32, Error>` -> `Some(i32)`
/// - `std::option::Option<&str>` -> `Some(&str)`
/// - `Tr::<G>::Ty` -> `None`
/// - `Tr::<G>::Gat<H>` -> `Some(H)`
/// - `u32` -> `None`
/// - `!` -> `None`
pub fn first_generic_arg(mut ty: &Type) -> Option<&Type> {
    fn inner(ty: &TypePath) -> Option<&Type> {
        let seg = last_punct(&ty.path.segments)?;
        if let PathArguments::AngleBracketed(a) = &seg.arguments
            && let GenericArgument::Type(ty) = first_punct(&a.args)?
        {
            Some(ty)
        } else {
            None
        }
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

/// Creates a `where` clause without any predicates.
pub fn empty_where_clause() -> WhereClause {
    WhereClause {
        where_token: <Token![where]>::default(),
        predicates: Punctuated::new(),
    }
}

/// Creates an expression representing the same literal string.
pub fn lit_str_expr(lit: &str) -> Expr {
    Expr::Lit(ExprLit {
        lit: Lit::Str(LitStr::new(lit, Span::call_site())),
        attrs: Vec::new(),
    })
}

/// Peels [`Expr::Group`] and [`Expr::Paren`] recursively to get the inner
/// expression.
pub fn unwrap_boxed_expr(mut expr: Box<Expr>) -> Box<Expr> {
    loop {
        match *expr {
            Expr::Group(g) => expr = g.expr,
            Expr::Paren(g) => expr = g.expr,
            _ => break expr,
        }
    }
}

pub fn to_field_transform(
    value: Box<Expr>,
    acc: &mut darling::error::Accumulator,
) -> Box<FieldTransform> {
    // using `_` as the type in error cases leads to less rustc follow-up errors
    // from type mismatches/incorrect types/unreachable code than using `Infallible`
    // or `!` or basically anything else. just one error that it's invalid.
    let value = unwrap_boxed_expr(value);
    let Expr::Closure(value) = *value else {
        let transform = FieldTransform {
            lifetimes: None,
            inputs: syn::parse_quote!(invalid_setter: _),
            body: syn::parse_quote!(invalid_setter),
        };

        acc.push(Error::custom("expected closure").with_span(&*value));
        return Box::new(transform);
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

    // map the `Pat` to their `PatType` and replace non-`Type` variants with dummies
    // and an error. also retain the commas for later, so they keep the right spans
    let inputs = value
        .inputs
        .into_pairs()
        .map(|pat| {
            let (pat, punct) = pat.into_tuple();
            let pat = match pat {
                Pat::Type(pat_type) => pat_type,
                pat => {
                    let err = Error::custom("closure inputs must all have an explicit type");
                    acc.push(err.with_span(&pat));
                    syn::parse_quote!(#pat: _)
                },
            };
            Pair::new(pat, punct)
        })
        .collect();

    let lifetimes = value.lifetimes.map(AngleBracketedGenerics::from);
    let body = value.body;

    Box::new(FieldTransform {
        lifetimes,
        inputs,
        body,
    })
}
