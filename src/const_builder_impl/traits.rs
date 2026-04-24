//! Contains emits for traits and trait-related functions.

use proc_macro2::TokenStream;

// CMBK breaking release: include `Default` impl for builder types unconditionally.
use super::EmitContext;

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
