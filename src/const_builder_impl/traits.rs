//! Contains emits for traits and trait-related functions.

use proc_macro2::TokenStream;
use syn::{GenericArgument, GenericParam, Ident, Path, PathArguments, Type};

use super::EmitContext;
use crate::model::FieldInfoSliceExt as _;
use crate::util::*;

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
pub fn emit_clone_simple(ctx: &EmitContext<'_>) -> TokenStream {
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
        GenericParam::Type(t) => Some(&t.ident),
        _ => None,
    });

    // if there are generic parameters, also look for associated parameters in
    // the form of `A::X` but not `<A as T>::X` in the field types, similar to
    // how the `Clone` derive does
    let mut bound_assoc_params = Vec::new();
    if bound_params.clone().next().is_some() {
        for field in fields.pub_api() {
            find_assocs_on(field.ty, &bound_params, &mut bound_assoc_params);
        }
    }

    // packed structs require `Copy` instead of `Clone` since we can't take
    // references to the fields to be able to call `Clone::clone` on them
    let (bound_trait, field_clone_map): (_, fn(_) -> _) = if !*packed {
        (quote::quote! { ::core::clone::Clone }, |f| {
            quote::quote! {
                // SAFETY: field is initialized and aligned
                ::core::clone::Clone::clone(unsafe {
                    &(*::core::mem::MaybeUninit::as_ptr(&self.unchecked.uninit)).#f
                })
            }
        })
    } else {
        (quote::quote! { ::core::marker::Copy }, |f| {
            quote::quote! {
                unsafe {
                    // abusing `*&` to ensure the field's type is actually `Copy`
                    // SAFETY: field is initialized and `Copy`
                    *&::core::ptr::read_unaligned(
                        &raw const (*::core::mem::MaybeUninit::as_ptr(&self.unchecked.uninit)).#f,
                    )
                }
            }
        })
    };

    let field_clones = field_idents.map(field_clone_map);
    let where_clause = RequiredWhereClause(where_clause);

    quote::quote! {
        #[automatically_derived]
        impl < #impl_generics #( const #field_generics1: ::core::primitive::bool ),* >
            ::core::clone::Clone for
            #builder < #ty_generics #(#field_generics2),* >
        #where_clause
            #( #bound_params: #bound_trait, )*
            #( #bound_assoc_params: #bound_trait, )*
        {
            #[inline]
            fn clone(&self) -> Self {
                let mut value = <#unchecked_builder < #ty_generics >>::new();

                #(
                    if #field_generics3 {
                        // SAFETY: const generic is true here, so the field must be initialized
                        value = value.#field_names(#field_clones);
                    }
                )*

                // SAFETY: fields that were claimed to be initialized were initialized again
                unsafe { value.assert_init() }
            }
        }
    }
}

fn find_assocs_on<'a, I>(t: &'a Type, find: &I, buf: &mut Vec<&'a Path>)
where
    I: Iterator<Item = &'a Ident> + Clone,
{
    match t {
        Type::Array(t) => find_assocs_on(&t.elem, find, buf),
        Type::Group(t) => find_assocs_on(&t.elem, find, buf),
        Type::Paren(t) => find_assocs_on(&t.elem, find, buf),
        Type::Slice(t) => find_assocs_on(&t.elem, find, buf),
        Type::Tuple(t) => t.elems.iter().for_each(|e| find_assocs_on(e, find, buf)),
        Type::Path(t) => {
            if t.qself.is_none()
                && let Some(first) = t.path.segments.first()
                && first.arguments.is_empty()
                && find.clone().any(|i| *i == first.ident)
            {
                buf.push(&t.path);
            }

            for pair in t.path.segments.pairs() {
                if let PathArguments::AngleBracketed(inner) = &pair.into_value().arguments {
                    for arg in inner.args.pairs() {
                        if let GenericArgument::Type(t) = arg.into_value() {
                            find_assocs_on(t, find, buf);
                        }
                    }
                }
            }
        },
        _ => {},
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
    let (bound_trait, bound_impl) = if !*packed {
        (
            simple_ident("CloneIf"),
            quote::quote! {
                #[automatically_derived]
                impl<T: ::core::clone::Clone> CloneIf<true> for T {
                    #[inline]
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
                    #[inline]
                    unsafe fn clone_unchecked(value: *const Self) -> Self {
                        // SAFETY: `value` is initialized and `Copy`
                        unsafe { ::core::ptr::read_unaligned(value) }
                    }
                }
            },
        )
    };

    let where_clause = RequiredWhereClause(where_clause);

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

            #bound_impl

            #[automatically_derived]
            impl < #impl_generics #( const #field_generics1: ::core::primitive::bool ),* >
                ::core::clone::Clone for
                #builder < #ty_generics #(#field_generics2),* >
            #where_clause
                #( #field_tys: #bound_trait < #field_generics3 >, )*
            {
                #[inline]
                fn clone(&self) -> Self {
                    let mut value = <#unchecked_builder < #ty_generics >>::new();

                    #(
                        if #field_generics4 {
                            // SAFETY: const generic is true here, so the field must be initialized
                            value = value.#field_names(unsafe {
                                #bound_trait::<#field_generics5>::clone_unchecked(
                                    &raw const (*::core::mem::MaybeUninit::as_ptr(&self.unchecked.uninit)).#field_idents,
                                )
                            });
                        }
                    )*

                    // SAFETY: fields that were claimed to be initialized were initialized again
                    unsafe { value.assert_init() }
                }
            }
        };
    }
}
