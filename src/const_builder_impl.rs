//! Actual impl for `#[derive(ConstBuilder)]`.
//!
//! This file contains setup and validation logic, then delegates to the actual
//! emit which is split into functions in its sub-modules.

use std::borrow::Cow;

use darling::util::Flag;
use darling::{Error, FromAttributes as _, FromDeriveInput as _};
use proc_macro2::{Span, TokenStream};
use quote::format_ident;
use syn::ext::IdentExt as _;
use syn::{Attribute, Data, Fields, FieldsNamed, Ident, Token, Visibility, WhereClause};

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
    // accumulate all errors here whenever possible. this allows us to both emit as
    // much correct code as possible while also eagerly emitting every error in the
    // input, providing better diagnostics for the user, and at least allowing
    // partial intellisense for the parts that we could generate.
    let mut acc = Error::accumulator();

    let builder_attrs = acc
        .handle(BuilderAttrs::from_derive_input(&input))
        .unwrap_or_default();

    let repr_attrs = acc
        .handle(ReprAttrs::from_derive_input(&input))
        .unwrap_or_default();

    // if we are dealing with wrong kind of item, no reason to continue, just error
    // out. we only continue for structs with named fields.
    let Some(raw_fields) = acc.handle(find_named_fields(&input.data)) else {
        return into_write_errors(acc);
    };

    let ty_generics = TypeGenerics(&input.generics.params);
    let impl_generics = ImplGenerics(&input.generics.params);
    let struct_generics = StructGenerics(&input.generics.params);

    let target_deprecated = find_deprecated(&input.attrs);
    let fields = load_fields(&input.ident, &builder_attrs, raw_fields, &mut acc);
    let where_clause = load_where_clause(&input.ident, ty_generics, input.generics.where_clause);

    let builder = load_builder_name(&input.ident, builder_attrs.rename);
    let builder_vis = builder_attrs.m_vis.unwrap_or(input.vis);
    let builder_fn = load_builder_fn_name(builder_attrs.rename_fn);

    let unchecked_builder =
        load_unchecked_builder_name(&input.ident, builder_attrs.unchecked.rename);
    let unchecked_builder_vis = builder_attrs.unchecked.vis.unwrap_or(Visibility::Inherited);

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
        packed: repr_attrs.packed.is_some(),
    };

    let mut output = base::emit_main(&ctx);
    output.extend(drop::emit_drop(&ctx));
    output.extend(fields::emit_fields(&ctx));
    output.extend(base::emit_builder_fn(&ctx));

    if builder_attrs.default.is_present() {
        output.extend(traits::emit_target_default(&ctx));
    }

    output.extend(unchecked::emit_unchecked(&ctx));

    if let Err(err) = acc.finish() {
        output.extend(err.write_errors());
    }

    output
}

fn load_builder_name(target: &Ident, rename: Option<Ident>) -> Ident {
    rename.unwrap_or_else(|| format_ident!("{}Builder", target))
}

fn load_builder_fn_name(rename: Option<BoolOr<Ident>>) -> Option<Ident> {
    match rename {
        None | Some(BoolOr::Bool(true)) => Some(simple_ident("builder")),
        Some(BoolOr::Bool(false)) => None,
        Some(BoolOr::Value(ident)) => Some(ident),
    }
}

fn load_unchecked_builder_name(target: &Ident, rename: Option<Ident>) -> Ident {
    rename.unwrap_or_else(|| format_ident!("{}UncheckedBuilder", target))
}

fn load_where_clause(
    target: &Ident,
    ty_generics: TypeGenerics<'_>,
    where_clause: Option<WhereClause>,
) -> WhereClause {
    let mut where_clause = where_clause.unwrap_or_else(empty_where_clause);
    let self_clause = syn::parse_quote!(#target < #ty_generics >: ::core::marker::Sized);
    where_clause.predicates.push(self_clause);
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

    for pair in raw_fields.named.pairs() {
        let raw_field = pair.into_value();
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
            if attrs.vis.is_some() {
                acc.push(skip_err("`skip` cannot be combined with `vis`"));
            }
            if attrs.setter.is_some() {
                acc.push(skip_err("`skip` cannot be combined with `setter`"));
            }
        }

        let name = attrs.rename.unwrap_or_else(|| ident.clone());

        // ensure correct ident formatting. overriding the span gets rid of a variable
        // name warning, probably because the span no longer points at the field.
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

        // need the empty line as a separate entry so rustdoc splits the paragraphs
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

        if setter.strip_option.is_present() && first_generic_arg(&raw_field.ty).is_none() {
            // best-effort type guessing and error message. if we get here, the emitted code
            // will fail to compile anyways, so this is just here to give slightly better
            // errors for some cases. note that this doesn't catch every case, f.e. if the
            // type is `PhantomData<u32>`, it will look fine here but error later, and due
            // to aliases, we can't really do much better.
            let err = Error::custom(
                "cannot determine element type for `strip_option`, use `Option<_>` directly",
            );
            acc.push(err.with_span(&setter.strip_option.span()));
        }

        let setter = if setter.strip_option.is_present() {
            FieldSetter::StripOption
        } else if let Some(transform) = setter.transform {
            FieldSetter::Transform(to_field_transform(transform, acc))
        } else {
            FieldSetter::Default
        };

        let vis = attrs.vis.unwrap_or_else(|| {
            if attrs.skip.is_present() {
                // this can only affect the unchecked builder's setter; specifying `vis` with
                // `skip` is disallowed. the reason it inherits this is that it only gives the
                // unchecked builder the method when the field would be accessible anyways.
                raw_field.vis.clone()
            } else {
                // assume `pub` by default since anything else would make the builder
                // potentially unusable in another module or crate.
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
            leak_on_drop: attrs.leak_on_drop.is_present(),
            unsized_tail: attrs.unsized_tail.is_present(),
            skip: attrs.skip.is_present(),
            setter,
        });
    }

    fields
}
