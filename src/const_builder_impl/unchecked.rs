//! Contains the emit for the unchecked builder type.
//!
//! This represents the core logic that the safe builder is built on top of.

use proc_macro2::TokenStream;
use syn::Token;
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

    let field_default_names = fields
        .iter()
        .filter(|f| f.default.is_some())
        .map(|f| &f.name);
    let field_default_values = fields.iter().filter_map(|f| f.default.as_deref());

    let field_setters = emit_unchecked_fields(ctx);
    let structure_check = emit_structure_check(ctx);

    let deprecated_field = fields.iter().find_map(|f| f.deprecated);
    let allow_deprecated_field = allow_deprecated(deprecated_field);

    quote::quote! {
        #[doc = #builder_doc]
        ///
        /// This version being _unchecked_ means it has less safety guarantees:
        /// - No tracking is done whether fields are initialized, so [`Self::build`] is `unsafe`.
        /// - If dropped, already initialized fields will be leaked.
        /// - The same field can be set multiple times. If done, the old value will be leaked.
        #[repr(transparent)]
        #[must_use = #BUILDER_MUST_USE]
        #target_deprecated
        #unchecked_builder_vis struct #unchecked_builder < #struct_generics > #where_clause {
            /// Honestly, don't use this directly.
            ///
            /// Using this field directly is equivalent to using [`Self::as_uninit`].
            inner: ::core::mem::MaybeUninit< #target < #ty_generics > >,
        }

        impl < #impl_generics > #unchecked_builder < #ty_generics > #where_clause {
            /// Creates a new unchecked builder.
            ///
            /// All default builder values will be set already.
            #[inline]
            pub const fn new() -> Self {
                #allow_deprecated_field
                Self { inner: ::core::mem::MaybeUninit::uninit() }
                #( . #field_default_names ( #field_default_values ) )*
            }

            /// Asserts that the fields specified by the const generics as well as all optional
            /// fields are initialized and promotes this value into a checked builder.
            ///
            /// # Safety
            ///
            /// The fields whose const generics are `true` and all optional (including skipped)
            /// fields must be initialized.
            ///
            /// Optional fields are initialized by [`Self::new`] by default, however using
            /// [`Self::as_uninit`] allows de-initializing them. This means that this function
            /// isn't even necessarily safe to call if all const generics are `false`.
            ///
            /// If the struct has been fully deinitialized previously (f.e. via
            /// `this.as_uninit() = MaybeUninit::uninit()`) and private fields are inaccessible,
            /// calling this function may always be unsound.
            #[inline]
            #builder_vis const unsafe fn assert_init < #(const #field_generics1: ::core::primitive::bool),* > (self) -> #builder < #ty_generics #(#field_generics2),* > {
                #builder {
                    inner: self,
                    _unsafe: (),
                }
            }

            /// Returns the finished value.
            ///
            /// # Safety
            ///
            /// _All_ fields must be initialized.
            ///
            /// Optional (including skipped) fields also must be initialized. Optional fields
            /// are initialized by [`Self::new`] by default, however using [`Self::as_uninit`]
            /// allows de-initializing them.
            ///
            /// If the struct has been fully deinitialized previously (f.e. via
            /// `this.as_uninit() = MaybeUninit::uninit()`) and private fields are inaccessible,
            /// calling this function may always be unsound.
            #[must_use = #BUILDER_BUILD_MUST_USE]
            #[inline]
            pub const unsafe fn build(self) -> #target < #ty_generics > {
                #structure_check

                unsafe {
                    // SAFETY: caller promises that all fields are initialized
                    ::core::mem::MaybeUninit::assume_init(self.inner)
                }
            }

            /// Gets a mutable reference to the partially initialized data.
            #[inline]
            pub const fn as_uninit(&mut self) -> &mut ::core::mem::MaybeUninit< #target < #ty_generics > > {
                &mut self.inner
            }

            #field_setters
        }
    }
}

fn emit_unchecked_fields(ctx: &EmitContext<'_>) -> TokenStream {
    let EmitContext { fields, packed, .. } = ctx;

    let mut output = TokenStream::new();

    let write_ident = if *packed {
        simple_ident("write_unaligned")
    } else {
        simple_ident("write")
    };

    // avoid giving the unsafe token the field's span so #![forbid(unsafe_code)] in
    // the caller isn't triggered by the macro expansion
    let unsafe_token = <Token![unsafe]>::default();

    for FieldInfo {
        ident,
        name,
        ty,
        vis,
        doc,
        deprecated,
        ..
    } in *fields
    {
        let allow_deprecated = allow_deprecated(*deprecated);

        output.extend(quote::quote_spanned! {ident.span()=>
            #(#doc)*
            #deprecated
            #[inline]
            // may trigger when field names begin with underscores
            #[allow(clippy::used_underscore_binding)]
            #vis const fn #name(mut self, value: #ty) -> Self
            where
                #ty: ::core::marker::Sized,
            {
                #unsafe_token {
                    // SAFETY: the value pointed to is in bounds of the object. if `repr(packed)`,
                    // this uses an unaligned write, otherwise the pointer is aligned for the value
                    ::core::ptr::#write_ident(
                        #allow_deprecated
                        &raw mut (*::core::mem::MaybeUninit::as_mut_ptr(&mut self.inner)).#ident,
                        value,
                    );
                }
                self
            }
        });
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
        // note: the goal here is to check that no other proc macro attribute added
        // `repr(packed)` in such a way that we didn't get to see it. emitting the
        // non-packed code for a packed struct would lead to UB.
        // the inverse, i.e. emitting packed code for a non-packed struct, however is
        // fine. that only adds a few restrictions and unaligned writes, so at worst
        // it's suboptimal, but still correct.
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
        const {
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
        }
    }
}
