//! Contains emits for traits and trait-related functions.

use proc_macro2::TokenStream;
use syn::GenericParam;

use super::EmitContext;
use crate::model::FieldInfoSliceExt as _;
use crate::util::simple_ident;

// CMBK const-traits: make trait impls const
pub fn emit_builder_default(ctx: &EmitContext<'_>) -> TokenStream {
    let EmitContext {
        builder,
        unchecked_builder,
        impl_generics,
        ty_generics,
        where_clause,
        ..
    } = ctx;

    quote::quote! {
        #[automatically_derived]
        impl < #impl_generics > ::core::default::Default for #builder < #ty_generics > #where_clause {
            /// Creates a new builder.
            #[inline]
            fn default() -> Self {
                Self::new()
            }
        }

        #[automatically_derived]
        impl < #impl_generics > ::core::default::Default for #unchecked_builder < #ty_generics > #where_clause  {
            /// Creates a new unchecked builder.
            ///
            /// No fields of the returned builder will be initialized.
            #[inline]
            fn default() -> Self {
                Self::new()
            }
        }
    }
}

// CMBK const-traits: make trait impl const, remove inherent function on
// breaking release.
pub fn emit_target_default(ctx: &EmitContext<'_>) -> TokenStream {
    let EmitContext {
        target,
        impl_generics,
        ty_generics,
        where_clause,
        ..
    } = ctx;

    quote::quote! {
        impl < #impl_generics > #target < #ty_generics > #where_clause {
            /// Creates the default for this type.
            #[inline]
            pub const fn default() -> Self {
                Self::builder().build()
            }
        }

        #[automatically_derived]
        impl < #impl_generics > ::core::default::Default for #target < #ty_generics > #where_clause {
            /// Creates the default for this type.
            #[inline]
            fn default() -> Self {
                Self::default()
            }
        }
    }
}

// CMBK const-traits: make trait impls const
pub fn emit_clone_like_derive(ctx: &EmitContext<'_>) -> TokenStream {
    let EmitContext {
        builder,
        unchecked_builder,
        impl_generics,
        ty_generics,
        where_clause,
        fields,
        packed,
        ..
    } = ctx;

    let field_names = fields.pub_api().map(|f| &f.name);
    let field_idents = fields.pub_api().map(|f| &f.ident);
    let field_generics1 = fields.gen_names();
    let field_generics2 = fields.gen_names();
    let field_generics3 = fields.gen_names();

    let bound_params = ty_generics.0.pairs().filter_map(|t| match t.into_value() {
        GenericParam::Type(t) => Some(t),
        _ => None,
    });

    // packed structs require `Copy` instead of `Clone` since we can't take
    // references to the fields to be able to call `Clone::clone` on them
    let (bound_trait, field_clone_map): (_, fn(_) -> _) = if !*packed {
        (quote::quote! { ::core::clone::Clone }, |f| {
            quote::quote! {
                // SAFETY: field is initialized and aligned
                ::core::clone::Clone::clone(unsafe {
                    &(*::core::mem::MaybeUninit::as_ptr(&self.inner.inner)).#f
                })
            }
        })
    } else {
        (quote::quote! { ::core::marker::Copy }, |f| {
            quote::quote! {
                unsafe {
                    // SAFETY: field is initialized and `Copy`
                    *&::core::ptr::read_unaligned(
                        &raw const (*::core::mem::MaybeUninit::as_ptr(&self.inner.inner)).#f,
                    )
                }
            }
        })
    };

    let field_clones = field_idents.map(field_clone_map);

    quote::quote! {
        #[automatically_derived]
        impl < #impl_generics #( const #field_generics1: ::core::primitive::bool ),* >
            ::core::clone::Clone for
            #builder < #ty_generics #(#field_generics2),* >
        #where_clause
            #(, #bound_params: #bound_trait )*
        {
            fn clone(&self) -> Self {
                let mut this = <#unchecked_builder < #ty_generics >>::new();

                #(
                    if #field_generics3 {
                        // SAFETY: const generic is true here, so the field must be initialized
                        this = this.#field_names(#field_clones);
                    }
                )*

                // SAFETY: fields that were claimed to be initialized were initialized again
                unsafe { this.assert_init() }
            }
        }
    }
}

// CMBK const-traits: make trait impls const
pub fn emit_clone_precise(ctx: &EmitContext<'_>) -> TokenStream {
    let EmitContext {
        builder,
        unchecked_builder,
        impl_generics,
        ty_generics,
        where_clause,
        fields,
        packed,
        ..
    } = ctx;

    let field_tys = fields.pub_api().map(|f| &f.ty);
    let field_names = fields.pub_api().map(|f| &f.name);
    let field_idents = fields.pub_api().map(|f| &f.ident);
    let field_generics1 = fields.gen_names();
    let field_generics2 = fields.gen_names();
    let field_generics3 = fields.gen_names();
    let field_generics4 = fields.gen_names();
    let field_generics5 = fields.gen_names();

    // packed structs require `Copy` instead of `Clone` since we can't take
    // references to the fields to be able to call `Clone::clone` on them
    let (bound_trait, clone_bound) = if !*packed {
        (
            simple_ident("CloneIf"),
            quote::quote! {
                #[automatically_derived]
                impl<T: ::core::clone::Clone> CloneIf<true> for T {
                    unsafe fn clone_unchecked(value: *const Self) -> Self {
                        // SAFETY: `value` is aligned and initialized
                        ::core::clone::Clone::clone(unsafe { &*value })
                    }
                }
            },
        )
    } else {
        (
            simple_ident("CopyIf"),
            quote::quote! {
                #[automatically_derived]
                impl<T: ::core::marker::Copy> CopyIf<true> for T {
                    unsafe fn clone_unchecked(value: *const Self) -> Self {
                        // SAFETY: `value` is initialized and `Copy`
                        unsafe { ::core::ptr::read_unaligned(value) }
                    }
                }
            },
        )
    };

    quote::quote! {
        const _: () = {
            // internal helper trait used for more precise bounds
            trait #bound_trait <const SET: ::core::primitive::bool>: ::core::marker::Sized {
                // # Safety
                // - `SET` must be `true`
                // - `value` must point to an initialized value of the right type
                // - `value` must be aligned (unless the `target` struct is packed)
                unsafe fn clone_unchecked(value: *const Self) -> Self;
            }

            #[automatically_derived]
            impl<T> #bound_trait <false> for T {
                unsafe fn clone_unchecked(value: *const Self) -> Self {
                    // SAFETY: const-generic is false, so this may not be called
                    unsafe { ::core::hint::unreachable_unchecked() }
                }
            }

            #clone_bound

            #[automatically_derived]
            impl < #impl_generics #( const #field_generics1: ::core::primitive::bool ),* >
                ::core::clone::Clone for
                #builder < #ty_generics #(#field_generics2),* >
            #where_clause
                #(, #field_tys: #bound_trait < #field_generics3 >)*
            {
                fn clone(&self) -> Self {
                    let mut this = <#unchecked_builder < #ty_generics >>::new();

                    #(
                        if #field_generics4 {
                            // SAFETY: const generic is true here, so the field must be initialized
                            this = this.#field_names(unsafe {
                                #bound_trait::<#field_generics5>::clone_unchecked(
                                    &raw const (*::core::mem::MaybeUninit::as_ptr(&self.inner.inner)).#field_idents,
                                )
                            });
                        }
                    )*

                    // SAFETY: fields that were claimed to be initialized were initialized again
                    unsafe { this.assert_init() }
                }
            }
        };
    }
}
