//! Emits the [`Drop`] impl for the safe builder type, ensuring that use at
//! runtime does not leak resources.

use proc_macro2::TokenStream;
use quote::format_ident;
use syn::spanned::Spanned as _;

use super::EmitContext;
use crate::model::*;

pub fn emit_drop(ctx: &EmitContext<'_>) -> TokenStream {
    let EmitContext {
        builder,
        impl_generics,
        ty_generics,
        where_clause,
        fields,
        ..
    } = ctx;

    let field_generics1 = fields.gen_names();
    let field_generics2 = fields.gen_names();

    let drop_inner = emit_drop_inner(ctx);

    quote::quote! {
        #[automatically_derived]
        impl < #impl_generics #( const #field_generics1: ::core::primitive::bool ),* > ::core::ops::Drop for #builder < #ty_generics #(#field_generics2),* > #where_clause {
            #[inline]
            fn drop(&mut self) {
                #drop_inner
            }
        }
    }
}

fn emit_drop_inner(ctx: &EmitContext<'_>) -> TokenStream {
    let EmitContext {
        unchecked_builder,
        impl_generics,
        ty_generics,
        where_clause,
        fields,
        packed,
        ..
    } = ctx;

    let body = emit_field_drops(ctx);

    // if the field drops emit an empty body, there will never be anything to drop
    // so we omit the entire nested function and the Drop impl becomes a noop
    if body.is_empty() {
        return TokenStream::new();
    }

    // pack the drop-flag generics into as few `u32` as possible,
    // then pass those to a non-generic `drop_inner` function.
    // most of the time, this will be just a single `u32`.
    let pack_size = 32;

    // only pack flags of fields that need a drop flag
    // leaked fields and `unsized_tail` of packed structs won't be dropped
    let dropped_fields = fields
        .pub_api()
        .filter(|f| !f.leak_on_drop && if *packed { !f.unsized_tail } else { true });

    let field_count = dropped_fields.clone().count();
    let mut field_vars = dropped_fields.clone().map(|f| &f.drop_flag);
    let mut field_var_generics = dropped_fields.map(|f| &f.gen_name);

    let pack_count = usize::div_ceil(field_count, pack_size);
    let packed_idents = (0..pack_count)
        .map(|i| format_ident!("packed_drop_flags_{i}"))
        .collect::<Vec<_>>();

    let mut pack = TokenStream::new();
    let mut unpack = TokenStream::new();

    for pack_ident in &packed_idents {
        let field_vars = field_vars.by_ref().take(pack_size);
        let field_var_generics = field_var_generics.by_ref().take(pack_size);

        let mask1 = (0usize..).map(|i| 1u32 << i);
        let mask2 = mask1.clone();

        pack.extend(quote::quote! {
            0 #( | if #field_var_generics { #mask1 } else { 0 } )*,
        });

        unpack.extend(quote::quote! {
            #( let #field_vars = (#pack_ident & #mask2) != 0; )*
        });
    }

    quote::quote! {
        #[cold]
        #[inline(never)]
        fn drop_inner < #impl_generics > (
            this: &mut #unchecked_builder < #ty_generics >,
            #( #packed_idents: ::core::primitive::u32 ),*
        ) #where_clause {
            #unpack
            #body
        }

        drop_inner(&mut self.inner, #pack);
    }
}

fn emit_field_drops(ctx: &EmitContext<'_>) -> TokenStream {
    let EmitContext { fields, packed, .. } = ctx;

    fn in_place_drop(
        FieldInfo {
            ident,
            drop_flag,
            ty,
            ..
        }: &FieldInfo,
    ) -> TokenStream {
        quote::quote! {
            // force const-eval to reduce debug binary size
            if const { ::core::mem::needs_drop::<#ty>() } && #drop_flag {
                unsafe {
                    // SAFETY: generics assert that this field is initialized and this is the last
                    // time this field will be read for this builder instance.
                    // struct is not `repr(packed)`, so the field must be aligned also.
                    ::core::ptr::drop_in_place(
                        &raw mut (*::core::mem::MaybeUninit::as_mut_ptr(&mut this.inner)).#ident,
                    );
                }
            }
        }
    }

    fn unaligned_drop(
        FieldInfo {
            ident,
            drop_flag,
            ty,
            ..
        }: &FieldInfo,
    ) -> TokenStream {
        quote::quote! {
            // force const-eval to reduce debug binary size
            if const { ::core::mem::needs_drop::<#ty>() } && #drop_flag {
                // triggers for non-copy, non-drop values
                #[allow(clippy::drop_non_drop)]
                unsafe {
                    // SAFETY: generics assert that this field is initialized and this is the last
                    // time this field will be read for this builder instance.
                    // fields of a packed struct cannot be dropped in-place due to alignment
                    ::core::mem::drop(::core::ptr::read_unaligned(
                        &raw mut (*::core::mem::MaybeUninit::as_mut_ptr(&mut this.inner)).#ident
                    ));
                }
            }
        }
    }

    fn expect_no_drop(FieldInfo { ident, ty, .. }: &FieldInfo) -> TokenStream {
        let message = format!("packed struct unsized tail field `{ident}` cannot be dropped");

        // this span puts the error message on the field type instead of the macro
        quote::quote_spanned! {ty.span()=>
            // caveat: rust does not actually guarantee that this returns `false` for types that
            // don't need to be dropped, but rustc still works that way so. also niche use case
            // that can be worked around with `leak_on_drop`.
            const {
                ::core::assert!(!::core::mem::needs_drop::<#ty>(), #message);
            }
        }
    }

    let mut output = TokenStream::new();

    // only fields with an exposed generic parameter will need to be dropped.
    // skipped fields only possibly have a default value, which is never dropped.
    for field in fields.pub_api() {
        if !field.leak_on_drop {
            output.extend(match (packed, field.unsized_tail) {
                (true, false) => unaligned_drop(field),
                (true, true) => expect_no_drop(field),
                (false, _) => in_place_drop(field),
            });
        }
    }

    output
}
