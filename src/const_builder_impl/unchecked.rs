//! Contains the emit for the unchecked builder type.
//!
//! This represents the core logic that the safe builder is built on top of.

use proc_macro2::TokenStream;
use syn::Ident;
use syn::spanned::Spanned as _;

use super::{BUILDER_BUILD_MUST_USE, BUILDER_MUST_USE, EmitContext};
use crate::model::*;
use crate::util::*;

pub fn emit_unchecked(ctx: &EmitContext<'_>) -> TokenStream {
    let EmitContext {
        target,
        target_deprecated,
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

    let builder_doc = format!("An _unchecked_ builder type for [`{target}`].");

    let field_generics1 = fields.gen_names();
    let field_generics2 = fields.gen_names();

    let field_setters = emit_unchecked_fields(ctx);
    let structure_check = emit_structure_check(ctx);

    quote::quote! {
        #[doc = #builder_doc]
        ///
        /// This version being _unchecked_ means it has less safety guarantees:
        ///
        /// - Initialized fields aren't tracked, so [`Self::build`] is unsafe.
        /// - Setting fields that were already initialized will [forget] the old value.
        /// - Default field values will not be automatically initialized.
        /// - Dropping it will [forget] all field values that were already set.
        ///
        /// [forget]: ::core::mem::forget
        #[repr(transparent)]
        #[must_use = #BUILDER_MUST_USE]
        #target_deprecated
        #unchecked_builder_vis struct #unchecked_builder < #struct_generics > #where_clause {
            /// Don't use this. Use [`Self::as_uninit`] instead.
            #[doc(hidden)]
            #[deprecated = "use `as_uninit` instead"]
            uninit: ::core::mem::MaybeUninit< #target < #ty_generics > >,
        }

        impl < #impl_generics > #unchecked_builder < #ty_generics > #where_clause {
            /// Creates a new unchecked builder.
            ///
            /// No fields of the returned builder will be initialized.
            #[inline]
            pub const fn new() -> Self {
                Self { uninit: ::core::mem::MaybeUninit::uninit() }
            }

            /// Asserts that the fields specified by the const generics as well as all optional
            /// fields are initialized and promotes this value into a checked builder.
            ///
            /// # Safety
            ///
            /// The fields whose const generics are `true` must be initialized.
            #[inline]
            #builder_vis const unsafe fn assert_init <
                #(const #field_generics1: ::core::primitive::bool),*
            > (self) -> #builder < #ty_generics #(#field_generics2),* > {
                #builder { unchecked: self }
            }

            /// Returns the finished value.
            ///
            /// # Safety
            ///
            /// _All_ fields must be initialized, including optional and skipped fields.
            ///
            /// If you want to initialize fields with their specified defaults, use the `*_default`
            /// methods on this builder before calling this method.
            #[must_use = #BUILDER_BUILD_MUST_USE]
            #[inline]
            pub const unsafe fn build(self) -> #target < #ty_generics > {
                // SAFETY: caller promises that all fields are initialized
                unsafe { ::core::mem::MaybeUninit::assume_init(self.uninit) }
            }

            /// Gets a mutable reference to the partially initialized data.
            #[inline]
            pub const fn as_uninit(&mut self) -> &mut ::core::mem::MaybeUninit< #target < #ty_generics > > {
                &mut self.uninit
            }

            #field_setters
        }

        #structure_check
    }
}

fn emit_unchecked_fields(ctx: &EmitContext<'_>) -> TokenStream {
    let EmitContext {
        target,
        fields,
        packed,
        ..
    } = ctx;

    let mut output = TokenStream::new();

    let write_ident = if *packed {
        simple_ident("write_unaligned")
    } else {
        simple_ident("write")
    };

    for FieldInfo {
        ident,
        name,
        ty,
        default,
        vis,
        deprecated,
        ..
    } in *fields
    {
        let doc = format!("Initializes the [`{target}::{ident}`] field.");
        let value = Ident::new("value", ident.span());

        output.extend(quote::quote! {
            #[doc = #doc]
            #deprecated
            #[inline]
            #vis const fn #name(mut self, #value: #ty) -> Self
            where
                #ty: ::core::marker::Sized,
            {
                unsafe {
                    // SAFETY: the value pointed to is in bounds of the object. if `repr(packed)`,
                    // this uses an unaligned write, otherwise the pointer is aligned for the value
                    ::core::ptr::#write_ident(
                        &raw mut (*::core::mem::MaybeUninit::as_mut_ptr(&mut self.uninit)).#ident,
                        #value,
                    );
                }
                self
            }
        });

        if let Some(default) = default {
            let allow_deprecated = allow_deprecated(*deprecated);
            let default_name = field_default_ident(name);
            let doc =
                format!("Initializes the [`{target}::{ident}`] field with its default value.");

            output.extend(quote::quote! {
                #[doc = #doc]
                #deprecated
                #[inline]
                #vis const fn #default_name(self) -> Self {
                    #allow_deprecated
                    self.#name(#default)
                }
            });
        }
    }

    output
}

fn emit_structure_check(ctx: &EmitContext<'_>) -> TokenStream {
    let EmitContext {
        target,
        impl_generics,
        ty_generics,
        where_clause,
        fields,
        packed,
        ..
    } = ctx;

    let field_idents1 = fields.iter().map(|f| f.ident);
    let field_idents2 = field_idents1.clone();
    let field_idents3 = field_idents1.clone();
    let field_tys1 = fields.iter().map(|f| f.ty);
    let field_tys2 = field_tys1.clone();

    let field_alignment_check = if *packed {
        TokenStream::new()
    } else {
        // note: the goal here is to check that no other proc macro attribute
        // added `repr(packed)` in such a way that we didn't get to see it.
        // emitting the non-packed code for a packed struct would lead to UB.
        // the inverse, i.e. emitting packed code for a non-packed struct,
        // however is fine. that only adds a few restrictions and unaligned
        // writes, so at worst it's suboptimal, but still correct.
        quote::quote! {
            fn _all_fields_aligned < #impl_generics > ( value: &#target < #ty_generics > ) #where_clause {
                #(_ = &value.#field_idents3;)*
            }
        }
    };

    quote::quote! {
        #[allow(
            // triggers if any field is deprecated, but that doesn't matter here
            deprecated,
            // these may trigger due to the signature and field/type names
            clippy::too_many_arguments,
            clippy::multiple_bound_locations,
            clippy::type_repetition_in_bounds,
            clippy::used_underscore_binding,
        )]
        const _: () = {
            // statically validate that the macro-seen fields match the final struct.
            // this ensures that the set of fields seen by the macro matches the final struct and
            // there is no undefined behavior due to asserting additional, uninitialized fields as
            // initialized because this macro didn't know about them.
            fn _derive_includes_every_field < #impl_generics > ( #( #field_idents1: #field_tys1 ),* ) -> #target < #ty_generics >
            #where_clause, #(#field_tys2: ::core::marker::Sized),*
            {
                #target { #(#field_idents2),* }
            }

            #field_alignment_check
        };
    }
}
