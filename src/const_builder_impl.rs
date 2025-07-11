use darling::{FromAttributes, FromDeriveInput};
use proc_macro2::TokenStream;
use quote::{ToTokens, format_ident};
use syn::ext::IdentExt as _;
use syn::{Data, Field, Fields, Ident, Token, Visibility, WhereClause};

use crate::model::*;
use crate::util::*;

struct EmitContext<'a> {
    target: Ident,
    builder: Ident,
    builder_vis: Visibility,
    unchecked_builder: Ident,
    unchecked_builder_vis: Visibility,
    impl_generics: ImplGenerics<'a>,
    ty_generics: TypeGenerics<'a>,
    where_clause: Option<&'a WhereClause>,
    fields: &'a [FieldInfo],
}

pub fn entry_point(input: syn::DeriveInput) -> syn::Result<TokenStream> {
    let builder_attrs = BuilderAttrs::from_derive_input(&input)?;

    let Data::Struct(data) = input.data else {
        return Err(syn::Error::new_spanned(input, "must be a struct"));
    };

    let Fields::Named(raw_fields) = data.fields else {
        return Err(syn::Error::new_spanned(
            data.fields,
            "must have named fields",
        ));
    };

    let fields = load_fields(&input.ident, &builder_attrs, raw_fields.named)?;

    let ctx = EmitContext {
        builder: format_ident!("{}Builder", input.ident),
        builder_vis: builder_attrs.m_vis.unwrap_or(input.vis),
        unchecked_builder: format_ident!("{}UncheckedBuilder", input.ident),
        unchecked_builder_vis: builder_attrs.unchecked.vis.unwrap_or(Visibility::Inherited),
        target: input.ident,
        impl_generics: ImplGenerics(&input.generics),
        ty_generics: TypeGenerics(&input.generics),
        where_clause: input.generics.where_clause.as_ref(),
        fields: &fields,
    };

    let mut output = emit_main(&ctx);
    output.extend(emit_fields(&ctx));

    if builder_attrs.default.is_present() {
        output.extend(emit_default(&ctx));
    }

    output.extend(emit_unchecked(&ctx));

    Ok(output)
}

fn emit_main(ctx: &EmitContext<'_>) -> TokenStream {
    let EmitContext {
        target,
        builder,
        builder_vis,
        unchecked_builder,
        unchecked_builder_vis,
        impl_generics,
        ty_generics,
        where_clause,
        fields,
    } = ctx;

    let builder_doc = format!("A builder type for [`{target}`].");

    let field_names = fields.names();
    let field_generics1 = fields.gen_names();
    let field_generics2 = fields.gen_names();
    let field_generics3 = fields.gen_names();
    let field_generics4 = fields.gen_names();
    let field_generics5 = fields.gen_names();
    let field_generics6 = fields.gen_names();

    let build_generics = fields
        .iter()
        .filter(|f| f.default.is_some())
        .map(|f| &f.gen_name);

    let build_args = fields.iter().map(|f| {
        if f.default.is_some() {
            let name = &f.gen_name;
            quote::quote! { #name }
        } else {
            quote::quote! { true }
        }
    });

    quote::quote! {
        #[doc = #builder_doc]
        #[repr(transparent)]
        #builder_vis struct #builder < #impl_generics #( const #field_generics1: bool = false ),* > #where_clause {
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

        impl < #impl_generics > #target < #ty_generics > #where_clause {
            /// Creates a new builder for this type.
            #builder_vis const fn builder() -> #builder < #ty_generics > {
                #builder::new()
            }
        }

        impl < #impl_generics > #builder < #ty_generics > #where_clause {
            /// Creates a new builder.
            pub const fn new() -> Self {
                // SAFETY: `new` initializes optional fields and no other
                // field is considered initialized by the const generics
                unsafe { #unchecked_builder::new().assert_init() }
            }
        }

        impl < #impl_generics #( const #field_generics2: bool ),* > #builder < #ty_generics #(#field_generics3),* > #where_clause {
            /// Unwraps this builder into its unsafe counterpart.
            ///
            /// This isn't unsafe in itself, however using it carelessly may lead to
            /// leaking objects and not dropping initialized values.
            #unchecked_builder_vis const fn into_unchecked(self) -> #unchecked_builder < #ty_generics > {
                // SAFETY: `inner` does not have drop glue or any memory invariants,
                // so we can copy it out safely. then, before returning it, we forget
                // `self` so its destructor doesn't run
                let inner = unsafe { ::core::ptr::read(&self.inner) };
                ::core::mem::forget(self);
                inner
            }
        }

        #[automatically_derived]
        impl < #impl_generics #( const #field_generics4: bool ),* > Drop for #builder < #ty_generics #(#field_generics5),* > #where_clause {
            fn drop(&mut self) {
                #(
                    if #field_generics6 {
                        unsafe {
                            // SAFETY: generics assert that this field is initialized
                            ::core::ptr::drop_in_place(
                                &raw mut (*::core::mem::MaybeUninit::as_mut_ptr(&mut self.inner.inner)).#field_names,
                            );
                        }
                    }
                )*
            }
        }

        impl < #impl_generics #( const #build_generics: bool ),* > #builder < #ty_generics #(#build_args),* > #where_clause {
            /// Returns the finished value.
            ///
            /// This function can only be called when all required fields have been set.
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

fn emit_default(ctx: &EmitContext<'_>) -> TokenStream {
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
            pub const fn default() -> Self {
                Self::builder().build()
            }
        }

        #[automatically_derived]
        impl < #impl_generics > ::core::default::Default for #target < #ty_generics > #where_clause {
            /// Creates the default for this type.
            fn default() -> Self {
                Self::default()
            }
        }
    }
}

fn emit_fields(ctx: &EmitContext<'_>) -> TokenStream {
    let EmitContext {
        builder,
        impl_generics,
        ty_generics,
        where_clause,
        fields,
        ..
    } = ctx;

    let mut output = TokenStream::new();

    for FieldInfo {
        name, ty, vis, doc, ..
    } in *fields
    {
        let set_generics = fields
            .iter()
            .filter(|f| f.name != *name)
            .map(|f| &f.gen_name);

        let set_args = fields.iter().map(|f| {
            if f.name == *name {
                quote::quote! { false }
            } else {
                let name = &f.gen_name;
                quote::quote! { #name }
            }
        });

        let post_set_args = fields.iter().map(|f| {
            if f.name == *name {
                quote::quote! { true }
            } else {
                let name = &f.gen_name;
                quote::quote! { #name }
            }
        });

        output.extend(quote::quote! {
            impl < #impl_generics #( const #set_generics: bool ),* > #builder < #ty_generics #(#set_args),* > #where_clause {
                #[doc = #doc]
                #vis const fn #name(self, value: #ty) -> #builder < #ty_generics #(#post_set_args),* > {
                    // SAFETY: same fields considered initialized, except `#name`,
                    // which is now considered initialized and actually initialized.
                    unsafe { self.into_unchecked().#name(value).assert_init() }
                }
            }
        });
    }

    output
}

fn emit_unchecked(ctx: &EmitContext<'_>) -> TokenStream {
    let EmitContext {
        target,
        builder,
        unchecked_builder,
        unchecked_builder_vis,
        impl_generics,
        ty_generics,
        where_clause,
        fields,
        ..
    } = ctx;

    let mut field_setters = TokenStream::new();

    for FieldInfo {
        name, ty, vis, doc, ..
    } in *fields
    {
        field_setters.extend(quote::quote! {
            #[doc = #doc]
            #vis const fn #name(mut self, value: #ty) -> #unchecked_builder < #ty_generics > {
                unsafe {
                    // SAFETY: address is in bounds
                    // when we return, the generics assert that the field is initialized
                    ::core::ptr::write(
                        &raw mut (*::core::mem::MaybeUninit::as_mut_ptr(&mut self.inner)).#name,
                        value,
                    );
                }
                self
            }
        });
    }

    let builder_doc = format!("An _unchecked_ builder type for [`{target}`].");

    let field_generics1 = fields.gen_names();
    let field_generics2 = fields.gen_names();

    let field_default_names = fields
        .iter()
        .filter(|f| f.default.is_some())
        .map(|f| &f.name);
    let field_default_values = fields.iter().filter_map(|f| f.default.as_ref());

    quote::quote! {
        #[doc = #builder_doc]
        ///
        /// This version being _unchecked_ means it has less safety guarantees:
        /// - No tracking is done whether fields are initialized, so [`Self::build`] is `unsafe`.
        /// - If dropped, already initialized fields will be leaked.
        /// - The same field can be set multiple times. If done, the old value will be leaked.
        #[repr(transparent)]
        #unchecked_builder_vis struct #unchecked_builder < #impl_generics > #where_clause {
            /// Honestly, don't use this directly.
            inner: ::core::mem::MaybeUninit< #target < #ty_generics > >,
        }

        impl < #impl_generics > #unchecked_builder < #ty_generics > #where_clause {
            /// Creates a new unchecked builder.
            pub const fn new() -> Self {
                let inner = ::core::mem::MaybeUninit::uninit();
                Self { inner }
                #( . #field_default_names ( #field_default_values ) )*
            }

            /// Asserts that the fields specified by the const generics are initialized
            /// and promotes this value into a checked builder.
            ///
            /// # Safety
            ///
            /// The fields whose const generic is `true` and optional fields have to be
            /// initialized.
            ///
            /// Optional fields are initialized by [`Self::new`] by default, however using
            /// [`Self::as_uninit`] allows de-initializing them.
            pub const unsafe fn assert_init < #(const #field_generics1: bool),* > (self) -> #builder < #ty_generics #(#field_generics2),* > {
                #builder {
                    inner: self,
                    _unsafe: (),
                }
            }

            /// Returns the finished value.
            ///
            /// # Safety
            ///
            /// This function requires that all fields have been initialized.
            pub const unsafe fn build(self) -> #target < #ty_generics > {
                unsafe {
                    // SAFETY: caller promises that all fields are initialized
                    ::core::mem::MaybeUninit::assume_init(self.inner)
                }
            }

            /// Gets a mutable reference to the partially initialized data.
            pub const fn as_uninit(&mut self) -> &mut ::core::mem::MaybeUninit< #target < #ty_generics > > {
                &mut self.inner
            }

            #field_setters
        }
    }
}

fn push_error(res: &mut syn::Result<()>, new: syn::Error) {
    match res {
        Ok(()) => *res = Err(new),
        Err(err) => err.combine(new),
    }
}

fn load_fields(
    target: &Ident,
    builder_attrs: &BuilderAttrs,
    raw_fields: impl IntoIterator<Item = Field>,
) -> syn::Result<Vec<FieldInfo>> {
    let mut errors = Ok(());
    let mut fields = Vec::new();
    for raw_field in raw_fields {
        let attrs = match FieldAttrs::from_attributes(&raw_field.attrs) {
            Ok(attrs) => attrs,
            Err(err) => {
                push_error(&mut errors, err.into());
                continue;
            },
        };

        if attrs.default.is_none() && builder_attrs.default.is_present() {
            push_error(
                &mut errors,
                syn::Error::new_spanned(
                    raw_field,
                    "structs with `#[builder(default)]` must provide a default value for all fields",
                ),
            );
            continue;
        }

        let name = raw_field.ident.expect("must be a named field here");
        let gen_name = format_ident!("_{}", name.unraw().to_string().to_uppercase());

        let doc = match &attrs.default {
            None => format!("Sets the [`{target}::{name}`] field."),
            Some(value) => format!(
                "Sets the [`{target}::{name}`] field.\n\nThis overrides the default of `{}`.",
                value.to_token_stream()
            ),
        };

        fields.push(FieldInfo {
            name,
            gen_name,
            ty: raw_field.ty,
            default: attrs.default,
            vis: attrs
                .vis
                .unwrap_or(Visibility::Public(<Token![pub]>::default())),
            doc,
        });
    }

    errors?;
    Ok(fields)
}
