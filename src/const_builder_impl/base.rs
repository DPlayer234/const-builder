//! Contains the safe builder type core.
//!
//! Field setters, drop logic, and optional trait impls are defined via
//! [`super::fields`] and [`super::drop`].

use proc_macro2::TokenStream;

use super::{BUILDER_BUILD_MUST_USE, BUILDER_MUST_USE, EmitContext};
use crate::model::*;
use crate::util::*;

pub fn emit_main(ctx: &EmitContext<'_>) -> TokenStream {
    let EmitContext {
        target,
        builder,
        builder_vis,
        unchecked_builder,
        unchecked_builder_vis,
        impl_generics,
        ty_generics,
        struct_generics,
        where_clause,
        fields,
        ..
    } = ctx;

    let t_true = simple_ident("true");
    let builder_doc = format!("A builder type for [`{target}`].");

    let field_generics1 = fields.gen_names();
    let field_generics2 = fields.gen_names();
    let field_generics3 = fields.gen_names();

    // exclude non-defaulted fields in `build` generic params
    // and require them to always be `true`
    let build_gens = fields
        .pub_api()
        .map(|f| f.default.is_some().then_some(&f.gen_name));

    let build_params = build_gens.clone().flatten();
    let build_args = build_gens.map(|f| f.unwrap_or(&t_true));

    quote::quote! {
        #[doc = #builder_doc]
        #[repr(transparent)]
        #[must_use = #BUILDER_MUST_USE]
        #builder_vis struct #builder < #struct_generics #( const #field_generics1: ::core::primitive::bool = false ),* > #where_clause {
            /// Inner unchecked builder. To move this value out, use [`Self::into_unchecked`].
            /// Honestly, don't use this directly.
            ///
            /// # Safety
            ///
            /// The fields specified by the const generics on [`Self`] have to be initialized in `inner`.
            inner: #unchecked_builder < #ty_generics >,
            /// Note that `inner` has a safety invariant.
            _unsafe: (),
        }

        impl < #impl_generics > #builder < #ty_generics > #where_clause {
            /// Creates a new builder.
            #[inline]
            pub const fn new() -> Self {
                // SAFETY: `new` initializes optional fields and no other
                // field is considered initialized by the const generics
                unsafe { #unchecked_builder::new().assert_init() }
            }
        }

        impl < #impl_generics #( const #field_generics2: ::core::primitive::bool ),* > #builder < #ty_generics #(#field_generics3),* > #where_clause {
            /// Unwraps this builder into its unsafe counterpart.
            ///
            /// This isn't unsafe in itself, however using it carelessly may lead to
            /// leaking objects and not dropping initialized values.
            #[inline]
            #unchecked_builder_vis const fn into_unchecked(self) -> #unchecked_builder < #ty_generics > {
                // the way this function is written tries to reduce the amount of runtime code
                // generated for unoptimized/debug builds without impacting optimized code.

                // this is morally equivalent to `ptr::read(&ManuallyDrop::new(self).inner)`, but
                // that isn't usably in const as of now. this is only needed to deconstruct `self`
                // because it has a `Drop` impl.

                // put `self` into `ManuallyDrop` so its destructor doesn't run
                let this = ::core::mem::ManuallyDrop::new(self);
                // `ManuallyDrop` is transparent over the inner type, so cast back
                let this = &raw const this as *const Self;
                // SAFETY: `self` won't be dropped so we can move out `inner` safely
                unsafe { ::core::ptr::read(&raw const (*this).inner) }
            }
        }

        impl < #impl_generics #( const #build_params: ::core::primitive::bool ),* > #builder < #ty_generics #(#build_args),* > #where_clause {
            /// Returns the finished value.
            ///
            /// This function can only be called when all required fields have been set.
            #[must_use = #BUILDER_BUILD_MUST_USE]
            #[inline]
            pub const fn build(self) -> #target < #ty_generics > {
                unsafe {
                    // SAFETY: generics assert that all required fields were initialized
                    // optional fields were set by `Self::new`.
                    self.into_unchecked().build()
                }
            }
        }
    }
}

pub fn emit_builder_fn(ctx: &EmitContext<'_>) -> TokenStream {
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
