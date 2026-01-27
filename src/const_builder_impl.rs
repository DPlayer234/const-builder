use std::borrow::Cow;
use std::slice;

use darling::util::Override;
use darling::{Error, FromAttributes as _, FromDeriveInput as _};
use proc_macro2::{Span, TokenStream};
use quote::{ToTokens, format_ident};
use syn::ext::IdentExt as _;
use syn::spanned::Spanned as _;
use syn::{Data, Fields, FieldsNamed, Ident, Token, Type, Visibility, WhereClause};

use crate::model::*;
use crate::util::*;

const BUILDER_MUST_USE: &str =
    "builders do nothing on their own and their methods return new values";
const BUILDER_BUILD_MUST_USE: &str = "dropping the return of `build` will just discard the inputs";

struct EmitContext<'a> {
    target: Ident,
    builder: Ident,
    builder_vis: Visibility,
    builder_fn: Option<Ident>,
    into_default: Ident,
    unset_field_ty: Ident,
    defaults_ty: Ident,
    impl_generics: ImplGenerics<'a>,
    ty_generics: TypeGenerics<'a>,
    struct_generics: StructGenerics<'a>,
    where_clause: WhereClause,
    fields: &'a [FieldInfo<'a>],
}

pub fn entry_point(input: syn::DeriveInput) -> TokenStream {
    // accumulate all errors here whenever possible. this allows us to both emit as
    // much as correct code as possible while also eagerly emitting every error in
    // the input, providing better diagnostics for the user, and at least allowing
    // partial intellisense for the parts that we could generate.
    let mut acc = Error::accumulator();

    let builder_attrs = acc
        .handle(BuilderAttrs::from_derive_input(&input))
        .unwrap_or_default();

    // if we are dealing with wrong kind of item, no reason to continue, just error
    // out. we only continue for structs with named fields.
    let Some(raw_fields) = acc.handle(find_named_fields(&input.data)) else {
        return into_write_errors(acc);
    };

    let ty_generics = TypeGenerics(&input.generics.params);
    let impl_generics = ImplGenerics(&input.generics.params);
    let struct_generics = StructGenerics(&input.generics.params);

    let fields = load_fields(&input.ident, &builder_attrs, raw_fields, &mut acc);
    let where_clause = load_where_clause(
        &input.ident,
        ty_generics,
        &fields,
        input.generics.where_clause,
    );

    let builder = load_builder_name(&input.ident, builder_attrs.rename);
    let builder_vis = builder_attrs.m_vis.unwrap_or(input.vis);
    let builder_fn = load_builder_fn_name(builder_attrs.rename_fn);

    let into_default = format_ident!("____{}__IntoDefault", builder, span = Span::call_site());
    let unset_field_ty = format_ident!("____{}__UnsetField", builder, span = Span::call_site());
    let defaults_ty = format_ident!("____{}__Defaults", builder, span = Span::call_site());

    let ctx = EmitContext {
        target: input.ident,
        builder,
        builder_vis,
        builder_fn,
        into_default,
        unset_field_ty,
        defaults_ty,
        impl_generics,
        ty_generics,
        struct_generics,
        where_clause,
        fields: &fields,
    };

    let mut output = emit_main(&ctx);
    output.extend(emit_field_defaults(&ctx));
    output.extend(emit_fields(&ctx));
    output.extend(emit_builder_fn(&ctx));

    if builder_attrs.default.is_present() {
        output.extend(emit_default(&ctx));
    }

    if let Err(err) = acc.finish() {
        output.extend(err.write_errors());
    }

    output
}

fn emit_main(ctx: &EmitContext<'_>) -> TokenStream {
    let EmitContext {
        target,
        builder,
        builder_vis,
        into_default,
        defaults_ty,
        unset_field_ty,
        impl_generics,
        ty_generics,
        struct_generics,
        where_clause,
        fields,
        ..
    } = ctx;

    let builder_doc = format!("A builder type for [`{target}`].");

    let new_args1 = fields.iter().map(|f| {
        if f.default.is_some() {
            defaults_ty
        } else {
            unset_field_ty
        }
    });
    let new_args2 = new_args1.clone();

    // exclude non-defaulted fields in `build` generic params
    // and require them to always be `*IntoDefault`
    let enumerated_defaults = fields
        .iter()
        .enumerate()
        .filter(|&(_index, f)| f.default.is_some());

    let build_params = enumerated_defaults.clone().map(|(_, f)| &f.gen_name);
    let build_args = fields.iter().map(|f| {
        if f.default.is_some() {
            &f.gen_name as &dyn ToTokens
        } else {
            &f.ty
        }
    });

    let build_where = enumerated_defaults.clone().map(|(index, f)| {
        let FieldInfo { gen_name, ty, .. } = f;
        quote::quote! { #gen_name: [const] #into_default < #ty_generics #index, Into = #ty > }
    });

    let fields_destruct = fields.idents();
    let fields_construct = fields.idents();

    let defaults = enumerated_defaults.map(|(index, f)| {
        let FieldInfo { ident, .. } = f;
        quote::quote! { let #ident = #into_default::< #ty_generics #index >::into_default(#ident); }
    });

    let deprecated_field = fields.iter().find_map(|f| f.deprecated);
    let allow_deprecated_field = allow_deprecated(deprecated_field);

    quote::quote! {
        /// Internal emit for `ConstBuilder`. No stability guarantees.
        #[doc(hidden)]
        #[allow(non_camel_case_types)]
        const trait #into_default< #impl_generics const ____INTERNAL_FIELD_INDEX: ::core::primitive::usize> {
            /// Internal emit for `ConstBuilder`. No stability guarantees.
            type Into;
            /// Internal emit for `ConstBuilder`. No stability guarantees.
            fn into_default(this: Self) -> Self::Into;
        }

        /// Internal emit for `ConstBuilder`. No stability guarantees.
        #[doc(hidden)]
        #[allow(non_camel_case_types)]
        #builder_vis struct #unset_field_ty < #impl_generics > (::core::marker::PhantomData<#target < #ty_generics >>) #where_clause;

        /// Internal emit for `ConstBuilder`. No stability guarantees.
        #[doc(hidden)]
        #[allow(non_camel_case_types)]
        #builder_vis struct #defaults_ty < #impl_generics > (::core::marker::PhantomData<#target < #ty_generics >>) #where_clause;

        #[doc = #builder_doc]
        #[derive(::core::clone::Clone)]
        #[must_use = #BUILDER_MUST_USE]
        #[allow(clippy::type_complexity)]
        #builder_vis struct #builder <
            #struct_generics
            _Fields = ( #(#new_args1 < #ty_generics > ,)* )
        > #where_clause {
            marker: ::core::marker::PhantomData<#target < #ty_generics >>,
            fields: _Fields,
        }

        impl < #impl_generics > #builder < #ty_generics > #where_clause {
            /// Creates a new builder.
            #[inline]
            pub const fn new() -> Self {
                Self {
                    marker: ::core::marker::PhantomData,
                    fields: ( #( #new_args2 (::core::marker::PhantomData), )* )
                }
            }
        }

        #[allow(clippy::type_complexity)]
        impl < #impl_generics #( #[allow(non_camel_case_types)] #build_params: ::core::marker::Sized ),* >
            #builder < #ty_generics ( #(#build_args,)* ) >
            #where_clause
        {
            /// Returns the finished value.
            ///
            /// This function can only be called when all required fields have been set.
            #[must_use = #BUILDER_BUILD_MUST_USE]
            #[inline]
            #[allow(clippy::used_underscore_binding)]
            pub const fn build(self) -> #target < #ty_generics >
            where
                #(#build_where),*
            {
                let Self {
                    marker: _,
                    fields: ( #(#fields_destruct,)* ),
                } = self;
                #(#defaults)*
                #allow_deprecated_field
                #target { #(#fields_construct,)* }
            }
        }
    }
}

fn emit_builder_fn(ctx: &EmitContext<'_>) -> TokenStream {
    let EmitContext {
        target,
        builder,
        builder_vis,
        builder_fn,
        impl_generics,
        ty_generics,
        where_clause,
        ..
    } = ctx;

    let Some(builder_fn) = builder_fn else {
        return TokenStream::new();
    };

    quote::quote! {
        impl < #impl_generics > #target < #ty_generics > #where_clause {
            /// Creates a new builder for this type.
            #[inline]
            #builder_vis const fn #builder_fn() -> #builder < #ty_generics > {
                #builder::new()
            }
        }
    }
}

fn emit_default(ctx: &EmitContext<'_>) -> TokenStream {
    let EmitContext {
        target,
        impl_generics,
        ty_generics,
        where_clause,
        ..
    } = ctx;

    quote::quote! {
        #[automatically_derived]
        impl < #impl_generics > const ::core::default::Default for #target < #ty_generics > #where_clause {
            /// Creates the default for this type.
            #[inline]
            fn default() -> Self {
                Self::builder().build()
            }
        }
    }
}

fn emit_fields(ctx: &EmitContext<'_>) -> TokenStream {
    let EmitContext {
        builder,
        impl_generics,
        ty_generics,
        where_clause,
        fields,
        ..
    } = ctx;

    let mut output = TokenStream::new();

    let store_ident = format_ident!("____move_but_ignore");

    for (
        index,
        FieldInfo {
            ident,
            name,
            gen_name,
            ty,
            vis,
            doc,
            deprecated,
            setter,
            ..
        },
    ) in fields.iter().enumerate()
    {
        let except_this = fields
            .iter()
            .enumerate()
            .map(|(i, f)| (i != index).then_some(f));

        // change generic argument for this field to `#f.ty`
        let post_set_args = except_this
            .clone()
            .map(|f| f.map(|f| &f.gen_name as &dyn ToTokens).unwrap_or(ty));

        let fields_destruct = except_this.map(|f| f.map(|f| f.ident).unwrap_or(&store_ident));
        let fields_construct = fields.idents();

        let mut ty = *ty;
        let (inputs, cast, tys, life, is_const) = split_setter(ident, setter, &mut ty);

        let const_token = is_const.then(<Token![const]>::default);
        let const_maybe_token = const_token.into_iter();

        output.extend(quote::quote_spanned! {ident.span()=>
            #(#doc)*
            #deprecated
            #[inline]
            // may occur with `transform` that specifies the same input ty for multiple parameters
            #[allow(clippy::type_repetition_in_bounds, clippy::multiple_bound_locations)]
            #vis #const_token fn #name #life (self, #inputs) -> #builder < #ty_generics (#(#post_set_args,)*) >
            where
                #(#gen_name: [#const_maybe_token] ::core::marker::Destruct,)*
                #(#tys: ::core::marker::Sized,)*
            {
                #cast
                let Self {
                    marker: _,
                    fields: ( #(#fields_destruct,)* ),
                } = self;
                #[allow(clippy::used_underscore_binding)]
                #builder {
                    marker: ::core::marker::PhantomData,
                    fields: ( #(#fields_construct,)* ),
                }
            }
        });
    }

    let field_params = fields.gen_names();
    let field_args = fields.gen_names();

    quote::quote! {
        #[allow(clippy::type_complexity)]
        impl < #impl_generics #( #[allow(non_camel_case_types)] #field_params: ::core::marker::Sized, )* >
            #builder < #ty_generics (#(#field_args,)*) >
            #where_clause
        {
            #output
        }
    }
}

// double-ref `ty` so we can return a slice without allocating for the common
// case and avoid cloning `Type` values for the transform cases that allocate a
// `Vec` of references. the outer ref is mutable so we can use it to store a ref
// to the inner `Option` type for the `strip_option` case.
fn split_setter<'t>(
    ident: &Ident,
    setter: &'t FieldSetter,
    ty: &'t mut &'t Type,
) -> (
    TokenStream,             // inputs
    Option<TokenStream>,     // cast
    Cow<'t, [&'t Type]>,     // tys
    Option<&'t TokenStream>, // life
    bool,                    // is-const
) {
    match setter {
        FieldSetter::Default => (
            quote::quote! { #ident: #ty },
            None,
            slice::from_ref(ty).into(),
            None,
            true,
        ),
        FieldSetter::StripOption => {
            *ty = first_generic_arg(ty).unwrap_or(ty);
            (
                quote::quote! { #ident: #ty },
                Some(quote::quote! { let #ident = ::core::option::Option::Some(#ident); }),
                slice::from_ref(ty).into(),
                None,
                true,
            )
        },
        FieldSetter::Transform(transform) => {
            let inputs = &transform.inputs;
            let body = &transform.body;
            (
                quote::quote! { #(#inputs),* },
                Some(quote::quote! { let #ident = #body; }),
                transform.inputs.iter().map(|t| &*t.ty).collect(),
                transform.lifetimes.as_ref(),
                transform.is_const,
            )
        },
    }
}

fn emit_field_defaults(ctx: &EmitContext<'_>) -> TokenStream {
    let EmitContext {
        into_default,
        defaults_ty,
        impl_generics,
        ty_generics,
        where_clause,
        fields,
        ..
    } = ctx;

    let mut output = TokenStream::new();

    for (
        index,
        FieldInfo {
            default,
            ty,
            non_const,
            ..
        },
    ) in fields.iter().enumerate()
    {
        let Some(default) = default else {
            continue;
        };

        output.extend(quote::quote! {
            impl < #impl_generics > const #into_default < #ty_generics #index > for #ty {
                type Into = Self;
                fn into_default(this: Self) -> Self {
                    this
                }
            }
        });

        let explicit_const = |default| {
            quote::quote! {
                impl < #impl_generics > const #into_default < #ty_generics #index > for #defaults_ty < #ty_generics > #where_clause {
                    type Into = #ty;
                    fn into_default(_: Self) -> #ty {
                        #default
                    }
                }
            }
        };

        let explicit_non_const = |default| {
            quote::quote! {
                impl < #impl_generics > #into_default < #ty_generics #index > for #defaults_ty < #ty_generics > #where_clause {
                    type Into = #ty;
                    fn into_default(_: Self) -> #ty {
                        #default
                    }
                }
            }
        };

        let inherit = || {
            quote::quote! {
                impl < #impl_generics > const #into_default < #ty_generics #index > for #defaults_ty < #ty_generics >
                #where_clause,
                    #ty: [const] ::core::default::Default
                {
                    type Into = #ty;
                    fn into_default(_: Self) -> #ty {
                        ::core::default::Default::default()
                    }
                }
            }
        };

        output.extend(match default {
            Override::Explicit(default) => {
                if *non_const {
                    explicit_non_const(default)
                } else {
                    explicit_const(default)
                }
            },
            Override::Inherit => inherit(),
        });
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

fn load_where_clause(
    target: &Ident,
    ty_generics: TypeGenerics<'_>,
    fields: &[FieldInfo<'_>],
    where_clause: Option<WhereClause>,
) -> WhereClause {
    let mut where_clause = where_clause.unwrap_or_else(empty_where_clause);
    let self_clause = syn::parse_quote!(#target < #ty_generics >: ::core::marker::Sized);
    where_clause.predicates.push(self_clause);

    for FieldInfo { ty, .. } in fields {
        let field_clause = syn::parse_quote!(#ty: ::core::marker::Sized);
        where_clause.predicates.push(field_clause);
    }

    where_clause
}

fn find_named_fields(data: &Data) -> darling::Result<&FieldsNamed> {
    if let Data::Struct(data) = data {
        if let Fields::Named(raw_fields) = &data.fields {
            return Ok(raw_fields);
        }
    }

    // just keep the call site span (i.e. the derive itself)
    Err(Error::custom(
        "`ConstBuilder` can only be derived for structs with named fields",
    ))
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

    for pair in raw_fields.named.pairs() {
        let raw_field = pair.into_value();

        let ident = raw_field
            .ident
            .as_ref()
            .expect("must be a named field here");

        let attrs = acc
            .handle(FieldAttrs::from_attributes(&raw_field.attrs))
            .unwrap_or_default();

        if attrs.default.is_none() && builder_attrs.default.is_present() {
            let err = Error::custom(
                "structs with `#[builder(default)]` must provide a default value for all fields",
            );
            acc.push(err.with_span(ident));
        }

        let name = attrs.rename.unwrap_or_else(|| ident.clone());

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

        let deprecated = find_deprecated(&raw_field.attrs);

        if attrs.setter.strip_option.is_present() && attrs.setter.transform.is_some() {
            let err = Error::custom(
                "may only specify one of the following `setter` fields: `strip_option`, `transform`",
            );
            acc.push(err.with_span(&attrs.setter.strip_option.span()));
        }

        if attrs.setter.strip_option.is_present() && first_generic_arg(&raw_field.ty).is_none() {
            // best-effort type guessing and error message. if we get here, the emitted code
            // will fail to compile anyways, so this is just here to give slightly better
            // errors for some cases. note that this doesn't catch every case, f.e. if the
            // type is `PhantomData<u32>`, it will look fine here but error later, and due
            // to aliases, we can't really do much better.
            let err = Error::custom(
                "cannot determine element type for `strip_option`, use `Option<_>` directly",
            );
            acc.push(err.with_span(&attrs.setter.strip_option.span()));
        }

        if attrs.non_const.is_present() && !matches!(attrs.default, Some(Override::Explicit(_))) {
            let err =
                Error::custom("`non_const` must be combined with an explicit `default` value");
            acc.push(err.with_span(&attrs.non_const.span()));
        }

        let setter = if attrs.setter.strip_option.is_present() {
            FieldSetter::StripOption
        } else if let Some(transform) = attrs.setter.transform {
            FieldSetter::Transform(to_field_transform(transform, acc))
        } else {
            FieldSetter::Default
        };

        fields.push(FieldInfo {
            ident,
            name,
            gen_name,
            ty: &raw_field.ty,
            default: attrs.default,
            vis: attrs
                .vis
                .unwrap_or(Visibility::Public(<Token![pub]>::default())),
            doc,
            deprecated,
            non_const: attrs.non_const.is_present(),
            setter,
        });
    }

    fields
}
