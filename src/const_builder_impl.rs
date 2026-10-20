//! Actual impl for `#[derive(ConstBuilder)]`.
//!
//! This file contains setup and validation logic, then delegates to the actual
//! emit which is split into functions in its sub-modules.

use std::borrow::Cow;

use darling::util::Flag;
use darling::{Error, FromAttributes as _};
use proc_macro2::{Span, TokenStream};
use quote::format_ident;
use syn::ext::IdentExt as _;
use syn::punctuated::Pair;
use syn::{
    Attribute, Data, Expr, Fields, FieldsNamed, GenericParam, Ident, Pat, ReturnType, Token, Type,
    Visibility, WhereClause,
};

use crate::model::*;
use crate::util::*;

mod base;
mod drop;
mod fields;
mod traits;
mod unchecked;

const BUILDER_MUST_USE: &str =
    "builders do nothing on their own and their methods return new values";
const BUILDER_BUILD_MUST_USE: &str = "dropping the return of `build` will just discard the inputs";

struct EmitContext<'a> {
    target: Ident,
    target_deprecated: Option<&'a Attribute>,
    builder: Ident,
    builder_vis: Visibility,
    builder_fn: Option<Ident>,
    unchecked_builder: Ident,
    unchecked_builder_vis: Visibility,
    impl_generics: ImplGenerics<'a>,
    ty_generics: TypeGenerics<'a>,
    struct_generics: StructGenerics<'a>,
    where_clause: WhereClause,
    fields: &'a [FieldInfo<'a>],
    packed: bool,
}

pub fn entry_point(input: syn::DeriveInput) -> TokenStream {
    // accumulate all errors here whenever possible. this allows us to both emit
    // as much correct code as possible while also eagerly emitting every error
    // in the input, providing better diagnostics for the user, and at least
    // allowing partial intellisense for the parts that we could generate.
    let mut acc = Error::accumulator();

    let builder_attrs = acc
        .handle(BuilderAttrs::from_attributes(&input.attrs))
        .unwrap_or_default();

    // if we are dealing with wrong kind of item, no reason to continue, just
    // error out. we only continue for structs with named fields.
    let Some(raw_fields) = acc.handle(find_named_fields(&input.data)) else {
        return into_write_errors(acc);
    };

    no_unexpected_attrs(&input, &mut acc);

    let ty_generics = TypeGenerics(&input.generics.params);
    let impl_generics = ImplGenerics(&input.generics.params);
    let struct_generics = StructGenerics(&input.generics.params);

    let target_deprecated = find_deprecated(&input.attrs);
    let fields = load_fields(&input.ident, &builder_attrs, raw_fields, &mut acc);

    let where_clause = load_where_clause(
        &input.ident,
        fields.last().is_some_and(|f| f.unsized_tail),
        ty_generics,
        input.generics.where_clause,
    );

    let builder = load_builder_name(&input.ident, builder_attrs.rename);
    let builder_vis = builder_attrs.r#pub.unwrap_or(input.vis);
    let builder_fn = load_builder_fn_name(builder_attrs.rename_fn);

    let unchecked_builder =
        load_unchecked_builder_name(&input.ident, builder_attrs.unchecked.rename);
    let unchecked_builder_vis = builder_attrs
        .unchecked
        .r#pub
        .unwrap_or(Visibility::Inherited);

    let packed = has_repr_packed(&input.attrs);

    let ctx = EmitContext {
        target: input.ident,
        target_deprecated,
        builder,
        builder_vis,
        builder_fn,
        unchecked_builder,
        unchecked_builder_vis,
        impl_generics,
        ty_generics,
        struct_generics,
        where_clause,
        fields: &fields,
        packed,
    };

    let mut output = base::emit_main(&ctx);
    output.push(drop::emit_drop(&ctx));
    output.push(fields::emit_fields(&ctx));
    output.push(base::emit_builder_fn(&ctx));

    if builder_attrs.default.is_present() {
        output.push(traits::emit_target_default(&ctx));
    }

    output.push(unchecked::emit_unchecked(&ctx));

    if let Some(clone_mode) = builder_attrs.clone {
        output.push(match clone_mode.unwrap_or_default() {
            CloneMode::Simple => traits::emit_clone_simple(&ctx),
            CloneMode::Precise => traits::emit_clone_precise(&ctx),
        });
    }

    output.push(traits::emit_builder_default(&ctx));

    if let Err(err) = acc.finish() {
        output.push(err.write_errors());
    }

    output
}

fn load_builder_name(target: &Ident, rename: Option<Ident>) -> Ident {
    rename.unwrap_or_else(|| format_ident!("{}Builder", target))
}

fn load_builder_fn_name(rename: MaybeIdent) -> Option<Ident> {
    match rename {
        MaybeIdent::None => Some(simple_ident("builder")),
        MaybeIdent::Some(ident) => Some(ident),
        MaybeIdent::Never => None,
    }
}

fn load_unchecked_builder_name(target: &Ident, rename: Option<Ident>) -> Ident {
    rename.unwrap_or_else(|| format_ident!("{}UncheckedBuilder", target))
}

fn load_where_clause(
    target: &Ident,
    is_unsized: bool,
    ty_generics: TypeGenerics<'_>,
    where_clause: Option<WhereClause>,
) -> WhereClause {
    let mut where_clause = where_clause.unwrap_or_else(empty_where_clause);
    if is_unsized {
        let self_clause = syn::parse_quote!(#target < #ty_generics >: ::core::marker::Sized);
        where_clause.predicates.push_value(self_clause);
        where_clause.predicates.push_punct(<Token![,]>::default());
    }
    where_clause
}

fn find_named_fields(data: &Data) -> darling::Result<&FieldsNamed> {
    if let Data::Struct(data) = data
        && let Fields::Named(raw_fields) = &data.fields
    {
        Ok(raw_fields)
    } else {
        // just keep the call site span (i.e. the derive itself)
        Err(Error::custom(
            "`ConstBuilder` can only be derived for structs with named fields",
        ))
    }
}

// this function accumulates errors so the field setters can be emitted for
// intellisense by the caller even when there are problems.
fn load_fields<'f>(
    target: &Ident,
    builder_attrs: &BuilderAttrs,
    raw_fields: &'f FieldsNamed,
    acc: &mut darling::error::Accumulator,
) -> Vec<FieldInfo<'f>> {
    let mut fields = Vec::with_capacity(raw_fields.named.len());
    let mut unsized_tail = Flag::default();

    for raw_field in raw_fields.named.values() {
        if unsized_tail.is_present() {
            let err = Error::custom(
                "`#[builder(unsized_tail)]` must be specified on the last field only",
            );
            acc.push(err.with_span(&unsized_tail.span()));
        }

        let ident = raw_field
            .ident
            .as_ref()
            .expect("must be a named field here");

        let deprecated = find_deprecated(&raw_field.attrs);

        let attrs = acc
            .handle(FieldAttrs::from_attributes(&raw_field.attrs))
            .unwrap_or_default();

        unsized_tail = attrs.unsized_tail;

        if attrs.default.is_none() && builder_attrs.default.is_present() {
            let err = Error::custom(
                "structs with `#[builder(default)]` must provide a default value for all fields",
            );
            acc.push(err.with_span(ident));
        }

        if attrs.skip.is_present() {
            let skip_err = |s| Error::custom(s).with_span(&attrs.skip.span());
            if attrs.default.is_none() {
                acc.push(skip_err("`skip` requires specifying `default`"));
            }
            if attrs.rename_generic.is_some() {
                acc.push(skip_err("`skip` cannot be combined with `rename_generic`"));
            }
            if attrs.r#pub.is_some() {
                acc.push(skip_err("`skip` cannot be combined with `pub`"));
            }
            if attrs.setter.is_some() {
                acc.push(skip_err("`skip` cannot be combined with `setter`"));
            }
        }

        let name = attrs.rename.unwrap_or_else(|| ident.clone());

        // ensure correct ident formatting. overriding the span gets rid of a
        // variable name warning, probably because the span no longer
        // points at the field.
        let drop_flag = format_ident!("drop_flag_{}", ident, span = Span::call_site());

        let gen_name = attrs.rename_generic.unwrap_or_else(|| {
            format_ident!(
                "_{}",
                ident.unraw().to_string().to_uppercase(),
                span = ident.span()
            )
        });

        let doc_header = match &attrs.default {
            None => format!("Sets the [`{target}::{ident}`] field."),
            Some(_) => {
                format!("Sets the [`{target}::{ident}`] field, replacing the default value.")
            },
        };

        // need the empty line as a separate entry so rustdoc splits the
        // paragraphs
        let mut doc = Vec::new();
        doc.push(Cow::Owned(doc_str_attr(&doc_header)));
        doc.push(Cow::Owned(doc_str_attr("")));
        doc.extend(iter_doc_attrs(&raw_field.attrs).map(Cow::Borrowed));

        let setter = attrs.setter.unwrap_or_default();

        if setter.strip_option.is_present() && setter.transform.is_some() {
            let err = Error::custom(
                "may only specify one of the following `setter` fields: `strip_option`, `transform`",
            );
            acc.push(err.with_span(&setter.strip_option.span()));
        }

        let setter = if setter.strip_option.is_present() {
            FieldSetter::StripOption(to_strip_option(&raw_field.ty, acc))
        } else if let Some(transform) = setter.transform {
            FieldSetter::Transform(to_field_transform(transform, acc))
        } else {
            FieldSetter::Default
        };

        let vis = attrs.r#pub.unwrap_or_else(|| {
            if attrs.skip.is_present() {
                // this can only affect the unchecked builder's setter;
                // specifying `pub` with `skip` is disallowed. the reason it
                // inherits this is that it only gives the unchecked builder the
                // method when the field would be accessible anyways.
                raw_field.vis.clone()
            } else {
                // assume `pub` by default since anything else would make the
                // builder potentially unusable in another module or crate.
                Visibility::Public(<Token![pub]>::default())
            }
        });

        fields.push(FieldInfo {
            ident,
            name,
            gen_name,
            drop_flag,
            ty: &raw_field.ty,
            default: attrs.default,
            vis,
            doc,
            deprecated,
            forget_on_drop: attrs.forget_on_drop.is_present(),
            unsized_tail: attrs.unsized_tail.is_present(),
            skip: attrs.skip.is_present(),
            setter,
        });
    }

    fields
}

/// Check and converts the field type to the inner stripped option type.
///
/// On failure, returns the outer type as a placeholder.
fn to_strip_option<'a>(ty: &'a Type, acc: &mut darling::error::Accumulator) -> &'a Type {
    // best-effort type guessing and error message. if we get here, the
    // emitted code will fail to compile anyways, so this is just here
    // to give slightly better errors for some cases. note that this
    // doesn't catch every case, f.e. if the type is `PhantomData<u32>`,
    // it will look fine here but error later, and due to aliases, we
    // can't really do much better.
    if let Some(inner_ty) = first_generic_arg(ty) {
        inner_ty
    } else {
        let err = Error::custom(
            "cannot determine element type for `strip_option`, use `Option<_>` directly",
        );
        acc.push(err.with_span(ty));
        ty
    }
}

/// Check and converts the expression accepted by `FieldSetterRaw::transform`.
///
/// On failure, returns a dummy or best-effort placeholder expression.
fn to_field_transform(
    value: Box<Expr>,
    acc: &mut darling::error::Accumulator,
) -> Box<FieldTransform> {
    // using `_` as the type in error cases leads to less rustc follow-up errors
    // from type mismatches/incorrect types/unreachable code than using
    // `Infallible` or `!` or basically anything else. just one error that
    // it's invalid.
    let value = peel_boxed_expr(value);
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

    if let Err(mod_err) = value.modifiers.require_empty() {
        let err = Error::custom("closure must not have any additional modifiers");
        acc.push(err.with_span(&mod_err.span()));
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

    // map the `Pat` to their `PatType` and replace non-`Type` variants with
    // dummies and an error. also retain the commas for later, so they keep
    // the right spans
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

// verify we check that attrs in so-far unused places are at least parsed and,
// if they aren't empty, rejected with appropriate errors
fn no_unexpected_attrs(input: &syn::DeriveInput, acc: &mut darling::error::Accumulator) {
    fn only_empty_builder(attrs: &[Attribute], acc: &mut darling::error::Accumulator) {
        _ = acc.handle(EmptyAttrs::from_attributes(attrs));
    }

    // considered moving `unsized_tail` to these, but that's insufficient
    // because it might be unsized due to an associated type being unsized
    for entry in input.generics.params.values() {
        match entry {
            GenericParam::Lifetime(param) => only_empty_builder(&param.attrs, acc),
            GenericParam::Type(param) => only_empty_builder(&param.attrs, acc),
            GenericParam::Const(param) => only_empty_builder(&param.attrs, acc),
        }
    }
}
