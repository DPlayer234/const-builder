//! Contains the emit for the safe field setters.

use std::borrow::Cow;
use std::slice;

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::spanned::Spanned as _;
use syn::{Token, Type};

use super::EmitContext;
use crate::model::*;
use crate::util::*;

pub fn emit_fields(ctx: &EmitContext<'_>) -> TokenStream {
    let EmitContext {
        builder,
        impl_generics,
        ty_generics,
        where_clause,
        fields,
        ..
    } = ctx;

    let mut output = TokenStream::new();

    let t_true = simple_ident("true");
    let t_false = simple_ident("false");

    // avoid giving the unsafe token the field's span so #![forbid(unsafe_code)] in
    // the caller isn't triggered by the macro expansion
    let unsafe_token = <Token![unsafe]>::default();

    for (
        index,
        FieldInfo {
            ident,
            name,
            ty,
            vis,
            doc,
            deprecated,
            setter,
            ..
        },
    ) in fields.pub_api().enumerate()
    {
        let used_gens = fields
            .pub_api()
            .enumerate()
            .map(|(i, f)| (i != index).then_some(&f.gen_name));

        // change generic argument for this field from `false` to `true`
        let pre_set_args = used_gens.clone().map(|o| o.unwrap_or(&t_false));
        let post_set_args = used_gens.clone().map(|o| o.unwrap_or(&t_true));

        // the generic parameters for the impl block exclude this field
        let set_params = used_gens.flatten();

        let allow_deprecated = allow_deprecated(*deprecated);

        let mut ty = *ty;
        let SplitSetter {
            inputs,
            cast,
            tys,
            life,
        } = split_setter(setter, &mut ty);

        output.extend(quote::quote_spanned! {ident.span()=>
            impl < #impl_generics #( const #set_params: ::core::primitive::bool ),* > #builder < #ty_generics #(#pre_set_args),* > #where_clause {
                #(#doc)*
                #deprecated
                #[inline]
                // may occur with `transform` that specifies the same input ty for multiple parameters
                #[allow(clippy::type_repetition_in_bounds, clippy::multiple_bound_locations)]
                #vis const fn #name #life (self, #inputs) -> #builder < #ty_generics #(#post_set_args),* >
                where
                    #(#tys: ::core::marker::Sized,)*
                {
                    #cast
                    // SAFETY: same fields considered initialized, except `#name`,
                    // which will be initialized by this call.
                    #allow_deprecated
                    #unsafe_token { self.into_unchecked().#name(value).assert_init() }
                }
            }
        });
    }

    output
}

// double-ref `ty` so we can return a slice without allocating for the common
// case and avoid cloning `Type` values for the transform cases that allocate a
// `Vec` of references. the outer ref is mutable so we can use it to store a ref
// to the inner `Option` type for the `strip_option` case.
fn split_setter<'t>(setter: &'t FieldSetter, ty: &'t mut &'t Type) -> SplitSetter<'t> {
    match setter {
        FieldSetter::Default => SplitSetter::simple(ty, None),
        FieldSetter::StripOption => {
            *ty = first_generic_arg(ty).unwrap_or(ty);
            let cast = quote::quote! { let value = ::core::option::Option::Some(value); };
            SplitSetter::simple(ty, Some(cast))
        },
        FieldSetter::Transform(transform) => SplitSetter::transform(transform),
    }
}

struct SplitSetter<'t> {
    inputs: SetterInputs<'t>,
    cast: Option<TokenStream>,
    tys: Cow<'t, [&'t Type]>,
    life: Option<&'t AngleBracketedGenerics>,
}

impl<'t> SplitSetter<'t> {
    fn simple(ty: &'t &'t Type, cast: Option<TokenStream>) -> Self {
        Self {
            inputs: SetterInputs::Value(ty),
            cast,
            tys: slice::from_ref(ty).into(),
            life: None,
        }
    }

    fn transform(transform: &'t FieldTransform) -> Self {
        let body = &*transform.body;
        let inputs = transform.inputs.pairs();
        Self {
            inputs: SetterInputs::Transform(transform),
            cast: Some(quote::quote! { let value = #body; }),
            tys: inputs.map(|t| &*t.into_value().ty).collect(),
            life: transform.lifetimes.as_ref(),
        }
    }
}

enum SetterInputs<'a> {
    Value(&'a Type),
    Transform(&'a FieldTransform),
}

impl ToTokens for SetterInputs<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match *self {
            // `value: #ty`
            SetterInputs::Value(ty) => {
                simple_ident("value").to_tokens(tokens);
                <Token![:]>::default().to_tokens(tokens);
                ty.to_tokens(tokens);
            },
            SetterInputs::Transform(inputs) => inputs.inputs.to_tokens(tokens),
        }
    }
}
