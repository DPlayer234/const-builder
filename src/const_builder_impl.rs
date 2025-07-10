use darling::util::Flag;
use darling::{FromAttributes, FromDeriveInput, FromMeta};
use proc_macro2::TokenStream;
use quote::{ToTokens, format_ident};
use syn::ext::IdentExt as _;
use syn::{Data, Expr, Fields, GenericParam, Generics, Ident, Token, Type, Visibility};

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(builder))]
struct BuilderAttrs {
    // darling recognizes `vis` as matching the struct visibility which i don't want
    #[darling(rename = "vis")]
    m_vis: Option<Visibility>,
    #[darling(default)]
    unchecked: BuilderUncheckedAttrs,
    default: Flag,
}

#[derive(Default, Debug, FromMeta)]
struct BuilderUncheckedAttrs {
    vis: Option<Visibility>,
}

#[derive(Default, Debug, FromAttributes)]
#[darling(attributes(builder))]
struct FieldAttrs {
    default: Option<Expr>,
    vis: Option<Visibility>,
}

struct FieldInfo {
    name: Ident,
    gen_name: Ident,
    ty: Type,
    default: Option<Expr>,
    vis: Visibility,
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

    let mut errors = Ok(());

    let mut fields = Vec::new();
    for raw_field in raw_fields.named {
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

        fields.push(FieldInfo {
            name,
            gen_name,
            ty: raw_field.ty,
            default: attrs.default,
            vis: attrs
                .vis
                .unwrap_or(Visibility::Public(<Token![pub]>::default())),
        });
    }

    errors?;

    let impl_generics = ImplGenerics(&input.generics);
    let ty_generics = TypeGenerics(&input.generics);
    let where_clause = &input.generics.where_clause;

    let target = &input.ident;

    let builder = format_ident!("{}Builder", input.ident);
    let builder_vis = builder_attrs.m_vis.unwrap_or(input.vis);
    let unchecked_builder = format_ident!("{}UncheckedBuilder", input.ident);
    let unchecked_builder_vis = builder_attrs.unchecked.vis.unwrap_or(Visibility::Inherited);

    // we just kinda need this iterator multiple times in the code below
    let field_names1 = fields.iter().map(|f| &f.gen_name);
    let field_names2 = field_names1.clone();
    let field_names3 = field_names1.clone();
    let field_names4 = field_names1.clone();
    let field_names5 = field_names1.clone();
    let field_names6 = field_names1.clone();
    let field_names7 = field_names1.clone();
    let field_names8 = field_names1.clone();

    let field_real_names = fields.iter().map(|f| &f.name);

    let field_default_names = fields
        .iter()
        .filter(|f| f.default.is_some())
        .map(|f| &f.name);
    let field_default_values = fields.iter().filter_map(|f| f.default.as_ref());

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

    let builder_doc = format!("A builder type for [`{target}`].");

    let mut output = quote::quote! {
        #[doc = #builder_doc]
        #[repr(transparent)]
        #builder_vis struct #builder < #impl_generics #( const #field_names1: bool = false ),* > #where_clause {
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

        impl < #impl_generics #( const #field_names2: bool ),* > #builder < #ty_generics #(#field_names3),* > #where_clause {
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
        impl < #impl_generics #( const #field_names4: bool ),* > Drop for #builder < #ty_generics #(#field_names5),* > #where_clause {
            fn drop(&mut self) {
                #(
                    if #field_names6 {
                        unsafe {
                            // SAFETY: generics assert that this field is initialized
                            ::core::ptr::drop_in_place(
                                &raw mut (*::core::mem::MaybeUninit::as_mut_ptr(&mut self.inner.inner)).#field_real_names,
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
    };

    if builder_attrs.default.is_present() {
        output.extend(quote::quote! {
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
        });
    }

    let mut field_setters = quote::quote! {};

    for field in &fields {
        let name = &field.name;
        let ty = &field.ty;
        let vis = &field.vis;

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

        let doc = match &field.default {
            None => format!("Sets the [`{target}::{name}`] field."),
            Some(value) => format!(
                "Sets the [`{target}::{name}`] field.\n\nThis overrides the default of `{}`.",
                value.to_token_stream()
            ),
        };

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

    output.extend(quote::quote! {
        #[doc = #builder_doc]
        ///
        /// This version is _unchecked_. Notably this means it has less guarantees:
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
            pub const unsafe fn assert_init < #(const #field_names7: bool),* > (self) -> #builder < #ty_generics #(#field_names8),* > {
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
    });

    Ok(output)
}

#[derive(Debug, Clone, Copy)]
struct ImplGenerics<'a>(&'a Generics);

#[derive(Debug, Clone, Copy)]
struct TypeGenerics<'a>(&'a Generics);

impl ToTokens for ImplGenerics<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for generic in &self.0.params {
            match generic {
                GenericParam::Lifetime(param) => param.to_tokens(tokens),
                GenericParam::Type(param) => {
                    param.ident.to_tokens(tokens);
                    param.colon_token.to_tokens(tokens);
                    param.bounds.to_tokens(tokens);
                },
                GenericParam::Const(param) => {
                    param.const_token.to_tokens(tokens);
                    param.ident.to_tokens(tokens);
                    param.colon_token.to_tokens(tokens);
                    param.ty.to_tokens(tokens);
                },
            }

            <Token![,]>::default().to_tokens(tokens);
        }
    }
}

impl ToTokens for TypeGenerics<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for generic in &self.0.params {
            match generic {
                GenericParam::Lifetime(param) => param.lifetime.to_tokens(tokens),
                GenericParam::Type(param) => param.ident.to_tokens(tokens),
                GenericParam::Const(param) => param.ident.to_tokens(tokens),
            }

            <Token![,]>::default().to_tokens(tokens);
        }
    }
}

fn push_error(res: &mut syn::Result<()>, new: syn::Error) {
    match res {
        Ok(()) => *res = Err(new),
        Err(err) => err.combine(new),
    }
}
