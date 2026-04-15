//! Contains the emit for the safe field setters.

use std::borrow::Cow;
use std::slice;

use proc_macro2::TokenStream;
use syn::Type;
use syn::spanned::Spanned as _;

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
        let (inputs, cast, tys, life) = split_setter(setter, &mut ty);

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
                    unsafe { self.into_unchecked().#name(value).assert_init() }
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
fn split_setter<'t>(
    setter: &'t FieldSetter,
    ty: &'t mut &'t Type,
) -> (
    TokenStream,             // inputs
    Option<TokenStream>,     // cast
    Cow<'t, [&'t Type]>,     // tys
    Option<&'t TokenStream>, // life
) {
    match setter {
        FieldSetter::Default => (
            quote::quote! { value: #ty },
            None,
            slice::from_ref(ty).into(),
            None,
        ),
        FieldSetter::StripOption => {
            *ty = first_generic_arg(ty).unwrap_or(ty);
            (
                quote::quote! { value: #ty },
                Some(quote::quote! { let value = ::core::option::Option::Some(value); }),
                slice::from_ref(ty).into(),
                None,
            )
        },
        FieldSetter::Transform(transform) => {
            let inputs = &transform.inputs;
            let body = &transform.body;
            (
                quote::quote! { #(#inputs),* },
                Some(quote::quote! { let value = #body; }),
                transform.inputs.iter().map(|t| &*t.ty).collect(),
                transform.lifetimes.as_ref(),
            )
        },
    }
}
