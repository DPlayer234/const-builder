use darling::ast::NestedMeta;
use darling::{Error, FromMeta};
use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident};
use syn::ext::IdentExt as _;
use syn::parse::{ParseStream, Parser as _};
use syn::punctuated::{Pair, Punctuated};
use syn::spanned::Spanned as _;
use syn::token::{Bracket, Paren};
use syn::{
    AttrStyle, Attribute, BoundLifetimes, Expr, ExprLit, GenericArgument, GenericParam, Ident, Lit,
    LitStr, Meta, MetaNameValue, Path, PathArguments, Token, Type, TypePath, TypeTuple,
    VisRestricted, Visibility, WhereClause,
};

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
        for generic in self.0.values() {
            match generic {
                GenericParam::Lifetime(param) => {
                    param.lifetime.to_tokens(tokens);
                    param.colon_token.to_tokens(tokens);
                    param.bounds.to_tokens(tokens);
                },
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
        for generic in self.0.values() {
            match generic {
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
        for generic in self.0.values() {
            match generic {
                GenericParam::Lifetime(param) => {
                    param.lifetime.to_tokens(tokens);
                    param.colon_token.to_tokens(tokens);
                    param.bounds.to_tokens(tokens);
                },
                GenericParam::Type(param) => {
                    param.ident.to_tokens(tokens);
                    param.colon_token.to_tokens(tokens);
                    param.bounds.to_tokens(tokens);
                    if let Some((eq_token, default)) = &param.default {
                        eq_token.to_tokens(tokens);
                        default.to_tokens(tokens);
                    }
                },
                GenericParam::Const(param) => {
                    param.const_token.to_tokens(tokens);
                    param.ident.to_tokens(tokens);
                    param.colon_token.to_tokens(tokens);
                    param.ty.to_tokens(tokens);
                    if let Some((eq_token, default)) = &param.default {
                        eq_token.to_tokens(tokens);
                        default.to_tokens(tokens);
                    }
                },
            }

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

/// Internal helper extension trait for [`TokenStream`].
pub trait TokenStreamExt {
    /// Pushes another token stream to the end of this one.
    fn push(&mut self, other: TokenStream);
}

impl TokenStreamExt for TokenStream {
    fn push(&mut self, other: TokenStream) {
        // some testing indicates picking the `Extend<TokenStream>` overload
        // performs slightly better than `Extend<TokenTree>`
        self.extend(std::iter::once(other));
    }
}

/// Internal helper extension trait for [`Punctuated`].
///
/// Mostly serves to avoid allocating on common functions.
pub trait PunctuatedExt {
    type Value;

    /// Allocation-free iterator over the values.
    fn values(&self) -> impl DoubleEndedIterator<Item = &Self::Value> + Clone;

    /// Returns the first value.
    fn first_value(&self) -> Option<&Self::Value> {
        self.values().next()
    }
    /// Returns the last value.
    fn last_value(&self) -> Option<&Self::Value> {
        self.values().next_back()
    }
}

impl<T, P> PunctuatedExt for Punctuated<T, P> {
    type Value = T;
    fn values(&self) -> impl DoubleEndedIterator<Item = &T> + Clone {
        // `pairs` doesn't allocate while `iter` does for some reason
        self.pairs().map(Pair::into_value)
    }
}

/// [`FromMeta`] value that accepts nothing, [`Ident`], or `!`.
#[derive(Default, Debug)]
pub enum MaybeIdent {
    #[default]
    None,
    Some(Ident),
    Never,
}

impl FromMeta for MaybeIdent {
    fn from_none() -> Option<Self> {
        Some(Self::None)
    }

    fn from_expr(expr: &Expr) -> darling::Result<Self> {
        Ident::from_expr(expr).map(Self::Some)
    }

    fn from_invalid_expr(mnv: &darling::ast::MetaNameValueInvalidExpr) -> darling::Result<Self> {
        match syn::parse2::<Token![!]>(mnv.value.clone()) {
            Ok(_) => Ok(Self::Never),
            Err(_) => {
                let span = if mnv.value.is_empty() {
                    mnv.path.span()
                } else {
                    mnv.value.span()
                };
                Err(Error::custom("expected identifier or `!`").with_span(&span))
            },
        }
    }
}

/// Ensures that a where-clause has been emitted.
pub struct RequiredWhereClause<'a>(pub &'a WhereClause);

impl ToTokens for RequiredWhereClause<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        self.0.where_token.to_tokens(tokens);
        self.0.predicates.to_tokens(tokens);
    }
}

/// Avoids reparsing `str` literals in `darling` derives.
///
/// Intended for the field defaults.
pub fn option_box_expr_without_reparse(meta: &Meta) -> darling::Result<Option<Box<Expr>>> {
    match meta {
        Meta::Path(_) => Err(Error::unsupported_format("path")),
        Meta::List(_) => Err(Error::unsupported_format("list")),
        Meta::NameValue(nv) => Ok(Some(Box::new(nv.value.clone()))),
    }
}

/// Helper that parses a [`Meta`] item as an optional [`Visibility`].
pub fn visibility_meta(meta: &Meta) -> darling::Result<Option<Visibility>> {
    match meta {
        Meta::Path(path) => Ok(Some(Visibility::Public(Token![pub](path.span())))),
        Meta::List(meta) => Ok(Some(Visibility::Restricted({
            let (in_token, path) = vis_restricted_inner_parser.parse2(meta.tokens.clone())?;
            VisRestricted {
                pub_token: Token![pub](meta.path.span()),
                paren_token: Paren(*meta.delimiter.span()),
                in_token,
                path,
            }
        }))),
        Meta::NameValue(_) => Err(Error::unsupported_format("expression")),
    }
}

fn vis_restricted_inner_parser(
    stream: ParseStream<'_>,
) -> syn::Result<(Option<Token![in]>, Box<Path>)> {
    if stream.peek(Token![in]) {
        let in_token: Token![in] = stream.parse()?;
        let path = stream.call(Path::parse_mod_style)?;
        Ok((Some(in_token), Box::new(path)))
    } else {
        let path = stream.call(Ident::parse_any)?;
        Ok((None, Box::new(path.into())))
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
    attrs.iter().filter(|f| is_path_ident(f.path(), "doc"))
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
    attrs.iter().find(|a| is_path_ident(a.path(), "deprecated"))
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

/// Returns an ident to be used as the default-value setter for a field.
pub fn field_default_ident(name: &Ident) -> Ident {
    format_ident!("{}_default", name)
}

/// Check if `path` is only `ident`.
///
/// Similar to [`Path::is_ident`] but also returns `true` if `path` is an
/// equivalent raw identifier.
fn is_path_ident(path: &Path, ident: &str) -> bool {
    path.get_ident().is_some_and(|path| {
        let path = path.to_string();
        let path = path.strip_prefix("r#").unwrap_or(&path);
        path == ident
    })
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
        let seg = ty.path.segments.last_value()?;
        if let PathArguments::AngleBracketed(a) = &seg.arguments
            && let GenericArgument::Type(ty) = a.args.first_value()?
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
pub fn peel_boxed_expr(mut expr: Box<Expr>) -> Box<Expr> {
    loop {
        match *expr {
            Expr::Group(g) => expr = g.expr,
            Expr::Paren(g) => expr = g.expr,
            _ => break expr,
        }
    }
}

/// Emits a `Sized` bound for `ty`.
pub fn sized_bound(ty: &Type) -> TokenStream {
    quote::quote! { #ty: ::core::marker::Sized }
}

/// Iterate immediately nested paths within the `meta` item.
///
/// The iterator will be empty if the nested meta parsing fails or `meta` isn't
/// a list.
fn iter_nested_paths(meta: &Meta) -> impl Iterator<Item = Path> {
    fn nested_to_path(meta: NestedMeta) -> Option<Path> {
        match meta {
            NestedMeta::Meta(Meta::Path(path)) => Some(path),
            NestedMeta::Meta(Meta::List(meta)) => Some(meta.path),
            NestedMeta::Meta(Meta::NameValue(meta)) => Some(meta.path),
            NestedMeta::NameValueInvalidExpr(meta) => Some(meta.path),
            NestedMeta::Lit(_) => None,
        }
    }

    if let Meta::List(meta) = meta
        && let Ok(meta) = NestedMeta::parse_meta_list(meta.tokens.clone())
    {
        meta.into_iter()
    } else {
        Default::default()
    }
    .filter_map(nested_to_path)
}

/// Determines if the attributes contain a `repr(packed)` annotation.
pub fn has_repr_packed(attrs: &[Attribute]) -> bool {
    attrs.iter().any(|a| {
        is_path_ident(a.path(), "repr")
            && iter_nested_paths(&a.meta).any(|p| is_path_ident(&p, "packed"))
    })
}

/// Finds associated types rooted by an ident matched with `find` and appends
/// their full path to `buf`.
///
/// Notably, this finds paths in the form of `A::X`, for which the `find`
/// closure receives `A`, but not `<A as T>::X`. This roughly matches the
/// behavior the standard derives have for their additional bounds.
pub fn find_assocs_on<'a>(t: &'a Type, find: impl Fn(&Ident) -> bool, buf: &mut Vec<&'a Path>) {
    return inner(t, &find, buf);

    type Find<'a> = dyn 'a + Fn(&Ident) -> bool;

    fn inner<'a>(t: &'a Type, find: &Find<'_>, buf: &mut Vec<&'a Path>) {
        match t {
            Type::Array(t) => inner(&t.elem, find, buf),
            Type::Group(t) => inner(&t.elem, find, buf),
            Type::Paren(t) => inner(&t.elem, find, buf),
            Type::Slice(t) => inner(&t.elem, find, buf),
            Type::Tuple(t) => inner_tuple(t, find, buf),
            Type::Path(t) => inner_path(t, find, buf),
            _ => {},
        }
    }

    fn inner_tuple<'a>(t: &'a TypeTuple, find: &Find<'_>, buf: &mut Vec<&'a Path>) {
        for e in &t.elems {
            inner(e, find, buf);
        }
    }

    fn inner_path<'a>(t: &'a TypePath, find: &Find<'_>, buf: &mut Vec<&'a Path>) {
        if t.qself.is_none()
            && let Some(first) = t.path.segments.first_value()
            && first.arguments.is_empty()
            && find(&first.ident)
        {
            buf.push(&t.path);
        }

        for seg in t.path.segments.values() {
            if let PathArguments::AngleBracketed(args) = &seg.arguments {
                for arg in args.args.values() {
                    if let GenericArgument::Type(t) = arg {
                        inner(t, find, buf);
                    }
                }
            }
        }
    }
}
