//! Contains the emit for the safe field setters.

use proc_macro2::TokenStream;
use quote::ToTokens;
use syn::spanned::Spanned as _;
use syn::{Ident, Token, Type};

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

    for (
        index,
        FieldInfo {
            ident,
            name,
            ty,
            vis,
            doc,
            deprecated,
            unsized_tail,
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

        let sized_bound = if *unsized_tail {
            Some(quote::quote! { #ty: ::core::marker::Sized, })
        } else {
            None
        };

        let SplitSetter {
            value,
            inputs,
            cast,
            life,
        } = split_setter(ident, setter, ty);

        output.extend(quote::quote! {
            impl < #impl_generics #( const #set_params: ::core::primitive::bool ),* >
                #builder < #ty_generics #(#pre_set_args),* >
            #where_clause
                #sized_bound
            {
                #(#doc)*
                #deprecated
                #[inline]
                // may occur with `transform` that specifies generics
                #[allow(clippy::multiple_bound_locations)]
                #vis const fn #name #life (self, #inputs) -> #builder < #ty_generics #(#post_set_args),* >
                {
                    #cast
                    // SAFETY: same fields considered initialized, except `#name`,
                    // which will be initialized by this call.
                    #allow_deprecated
                    unsafe { self.into_unchecked().#name(#value).assert_init() }
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
fn split_setter<'t>(ident: &Ident, setter: &'t FieldSetter, ty: &'t Type) -> SplitSetter<'t> {
    match setter {
        FieldSetter::Default => SplitSetter::simple(ident, ty, None),
        FieldSetter::StripOption => {
            let ty = first_generic_arg(ty).unwrap_or(ty);
            let cast = quote::quote! { let value = ::core::option::Option::Some(value); };
            SplitSetter::simple(ident, ty, Some(cast))
        },
        FieldSetter::Transform(transform) => SplitSetter::transform(transform),
    }
}

struct SplitSetter<'t> {
    value: Ident,
    inputs: SetterInputs<'t>,
    cast: Option<TokenStream>,
    life: Option<&'t AngleBracketedGenerics>,
}

impl<'t> SplitSetter<'t> {
    fn simple(ident: &Ident, ty: &'t Type, cast: Option<TokenStream>) -> Self {
        let value = Ident::new("value", ident.span());
        Self {
            value: value.clone(),
            inputs: SetterInputs::Value(value, ty),
            cast,
            life: None,
        }
    }

    fn transform(transform: &'t FieldTransform) -> Self {
        let value = Ident::new("value", transform.body.span());
        let body = &*transform.body;
        Self {
            inputs: SetterInputs::Transform(transform),
            cast: Some(quote::quote! { let #value = #body; }),
            life: transform.lifetimes.as_ref(),
            value,
        }
    }
}

enum SetterInputs<'a> {
    Value(Ident, &'a Type),
    Transform(&'a FieldTransform),
}

impl ToTokens for SetterInputs<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            // `value: #ty`
            SetterInputs::Value(value, ty) => {
                value.to_tokens(tokens);
                <Token![:]>::default().to_tokens(tokens);
                ty.to_tokens(tokens);
            },
            SetterInputs::Transform(inputs) => inputs.inputs.to_tokens(tokens),
        }
    }
}
