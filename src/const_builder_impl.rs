use darling::{FromAttributes as _, FromDeriveInput as _};
use proc_macro2::{Span, TokenStream};
use quote::format_ident;
use syn::ext::IdentExt as _;
use syn::{Data, Field, Fields, Ident, Token, Visibility, WhereClause};

use crate::model::*;
use crate::util::*;

const BUILDER_MUST_USE: &str =
    "builders do nothing on their own and their methods return new values";
const BUILDER_BUILD_MUST_USE: &str = "dropping the return of `build` will just discard the inputs";

struct EmitContext<'a> {
    target: Ident,
    builder: Ident,
    builder_vis: Visibility,
    builder_fn: Option<Ident>,
    unchecked_builder: Ident,
    unchecked_builder_vis: Visibility,
    impl_generics: ImplGenerics<'a>,
    ty_generics: TypeGenerics<'a>,
    where_clause: WhereClause,
    fields: &'a [FieldInfo],
    packed: bool,
}

struct DropPack {
    pack_ty: TokenStream,
    pack: TokenStream,
    unpack: TokenStream,
}

pub fn entry_point(input: syn::DeriveInput) -> syn::Result<TokenStream> {
    let builder_attrs = BuilderAttrs::from_derive_input(&input)?;
    let repr_attrs = ReprAttrs::from_derive_input(&input)?;

    let Data::Struct(data) = input.data else {
        return Err(syn::Error::new_spanned(
            input.ident,
            "`ConstBuilder` can only be derived for structs",
        ));
    };

    let Fields::Named(raw_fields) = data.fields else {
        return Err(syn::Error::new_spanned(
            data.fields,
            "`ConstBuilder` can only be derived for structs with named fields",
        ));
    };

    let ty_generics = TypeGenerics(&input.generics.params);
    let impl_generics = ImplGenerics(&input.generics.params);

    let (fields, errors) = load_fields(&input.ident, &builder_attrs, raw_fields.named);
    let where_clause = load_where_clause(&input.ident, ty_generics, input.generics.where_clause);

    let builder = load_builder_name(&input.ident, builder_attrs.rename);
    let builder_vis = builder_attrs.m_vis.unwrap_or(input.vis);
    let builder_fn = load_builder_fn_name(builder_attrs.rename_fn);

    let unchecked_builder =
        load_unchecked_builder_name(&input.ident, builder_attrs.unchecked.rename);
    let unchecked_builder_vis = builder_attrs.unchecked.vis.unwrap_or(Visibility::Inherited);

    let ctx = EmitContext {
        target: input.ident,
        builder,
        builder_vis,
        builder_fn,
        unchecked_builder,
        unchecked_builder_vis,
        impl_generics,
        ty_generics,
        where_clause,
        fields: &fields,
        packed: repr_attrs.packed.is_present(),
    };

    let mut output = emit_main(&ctx);
    output.extend(emit_drop(&ctx));
    output.extend(emit_fields(&ctx));
    output.extend(emit_builder_fn(&ctx));

    if builder_attrs.default.is_present() {
        output.extend(emit_default(&ctx));
    }

    output.extend(emit_unchecked(&ctx));

    if let Err(err) = errors {
        output.extend(err.into_compile_error());
    }

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
        ..
    } = ctx;

    let builder_doc = format!("A builder type for [`{target}`].");

    let field_generics1 = fields.gen_names();
    let field_generics2 = fields.gen_names();
    let field_generics3 = fields.gen_names();

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
        #[must_use = #BUILDER_MUST_USE]
        #builder_vis struct #builder < #impl_generics #( const #field_generics1: ::core::primitive::bool = false ),* > #where_clause {
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
                // SAFETY: `inner` does not have drop glue or any memory invariants,
                // so we can copy it out safely. then, before returning it, we forget
                // `self` so its destructor doesn't run
                let inner = unsafe { ::core::ptr::read(&self.inner) };
                ::core::mem::forget(self);
                inner
            }
        }

        impl < #impl_generics #( const #build_generics: ::core::primitive::bool ),* > #builder < #ty_generics #(#build_args),* > #where_clause {
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

fn emit_builder_fn(ctx: &EmitContext<'_>) -> TokenStream {
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

fn emit_drop(ctx: &EmitContext<'_>) -> TokenStream {
    let EmitContext {
        builder,
        unchecked_builder,
        impl_generics,
        ty_generics,
        where_clause,
        fields,
        ..
    } = ctx;

    let body = emit_field_drops(ctx);

    let field_generics1 = fields.gen_names();
    let field_generics2 = fields.gen_names();

    let DropPack {
        pack_ty,
        pack,
        unpack,
    } = emit_field_drop_pack(ctx);

    quote::quote! {
        #[automatically_derived]
        impl < #impl_generics #( const #field_generics1: ::core::primitive::bool ),* > Drop for #builder < #ty_generics #(#field_generics2),* > #where_clause {
            #[inline]
            fn drop(&mut self) {
                #[cold]
                #[inline(never)]
                fn drop_inner < #impl_generics > (this: &mut #unchecked_builder < #ty_generics >, packed: #pack_ty) #where_clause {
                    #unpack
                    #body
                }

                drop_inner(&mut self.inner, #pack);
            }
        }
    }
}

fn emit_field_drop_pack(ctx: &EmitContext<'_>) -> DropPack {
    let EmitContext { fields, .. } = ctx;

    let dropped_fields = fields.iter().filter(|f| !f.unsized_tail && !f.leak_on_drop);
    let field_count = dropped_fields.clone().count();

    // nothing to drop, we just emit an empty pack
    if field_count == 0 {
        return DropPack {
            pack_ty: quote::quote! { () },
            pack: quote::quote! { () },
            unpack: TokenStream::new(),
        };
    }

    let field_vars = dropped_fields.clone().map(|f| &f.drop_flag);
    let field_var_generics = dropped_fields.map(|f| &f.gen_name);

    // for 32 or fewer fields, we can pack the flags into a single `u32`
    if field_count <= 32 {
        let indices = 0..field_count;
        let mask = (0..field_count).map(|i| 1u32 << i);

        return DropPack {
            pack: quote::quote! {
                const { 0 #( | (#field_var_generics as u32) << #indices )* }
            },
            unpack: quote::quote! {
                #( let #field_vars = (packed & #mask) != 0; )*
            },
            pack_ty: quote::quote! { ::core::primitive::u32 },
        };
    }

    // for more flags, emit packing into an array of usizes as small as possible for
    // the target architecture. which requires emiting more complex types.
    let usize_bits = quote::quote! {
        (::core::primitive::usize::BITS as ::core::primitive::usize)
    };
    let pack_count = quote::quote! {
        { ::core::primitive::usize::div_ceil(#field_count, #usize_bits) }
    };
    let pack_ty = quote::quote! {
        [::core::primitive::usize; #pack_count]
    };

    DropPack {
        // pack init flag generics into as few usizes as possible to minimize the amount of
        // code needed to pass them to `drop_inner`. usually, this will be just 1.
        pack_ty,
        pack: quote::quote! {
            const {
                let flags = [#(#field_var_generics),*];
                let mut packed = [0usize; #pack_count];
                let mut index = 0usize;
                while index < #field_count {
                    if flags[index] {
                        packed[index / #usize_bits] |= 1usize << (index % #usize_bits);
                    }
                    index += 1;
                }
                packed
            }
        },
        unpack: quote::quote! {
            let [#(#field_vars),*] = ::core::array::from_fn(|i| (packed[i / #usize_bits] & (1 << (i % #usize_bits))) != 0);
        },
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
            if #drop_flag && ::core::mem::needs_drop::<#ty>() {
                unsafe {
                    // SAFETY: generics assert that this field is initialized
                    // struct is not `repr(packed)`, so the field must be aligned also
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
            if #drop_flag && ::core::mem::needs_drop::<#ty>() {
                unsafe {
                    // SAFETY: generics assert that this field is initialized
                    ::core::mem::drop(::core::ptr::read_unaligned(
                        &raw mut (*::core::mem::MaybeUninit::as_mut_ptr(&mut this.inner)).#ident
                    ));
                }
            }
        }
    }

    fn expect_no_drop(FieldInfo { ident, ty, .. }: &FieldInfo) -> TokenStream {
        let message = format!("unsized tail field {ident} cannot be dropped");
        quote::quote! {
            const {
                assert!(!::core::mem::needs_drop::<#ty>(), #message);
            }
        }
    }

    let mut output = TokenStream::new();

    for field in *fields {
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

    for (
        index,
        FieldInfo {
            name, ty, vis, doc, ..
        },
    ) in fields.iter().enumerate()
    {
        let set_generics = fields
            .iter()
            .enumerate()
            .filter_map(|(i, f)| (i != index).then_some(&f.gen_name));

        let set_args = fields.iter().enumerate().map(|(i, f)| {
            if i == index {
                quote::quote! { false }
            } else {
                let name = &f.gen_name;
                quote::quote! { #name }
            }
        });

        let post_set_args = fields.iter().enumerate().map(|(i, f)| {
            if i == index {
                quote::quote! { true }
            } else {
                let name = &f.gen_name;
                quote::quote! { #name }
            }
        });

        output.extend(quote::quote! {
            impl < #impl_generics #( const #set_generics: ::core::primitive::bool ),* > #builder < #ty_generics #(#set_args),* > #where_clause {
                #(#[doc = #doc])*
                #[inline]
                #vis const fn #name(self, value: #ty) -> #builder < #ty_generics #(#post_set_args),* >
                where
                    #ty: ::core::marker::Sized,
                {
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
        builder_vis,
        unchecked_builder,
        unchecked_builder_vis,
        impl_generics,
        ty_generics,
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
    let field_default_values = fields.iter().filter_map(|f| f.default.as_ref());

    let field_setters = emit_unchecked_fields(ctx);

    quote::quote! {
        #[doc = #builder_doc]
        ///
        /// This version being _unchecked_ means it has less safety guarantees:
        /// - No tracking is done whether fields are initialized, so [`Self::build`] is `unsafe`.
        /// - If dropped, already initialized fields will be leaked.
        /// - The same field can be set multiple times. If done, the old value will be leaked.
        #[repr(transparent)]
        #[must_use = #BUILDER_MUST_USE]
        #unchecked_builder_vis struct #unchecked_builder < #impl_generics > #where_clause {
            /// Honestly, don't use this directly.
            inner: ::core::mem::MaybeUninit< #target < #ty_generics > >,
        }

        impl < #impl_generics > #unchecked_builder < #ty_generics > #where_clause {
            /// Creates a new unchecked builder.
            #[inline]
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
            /// This function requires that all fields have been initialized.
            #[must_use = #BUILDER_BUILD_MUST_USE]
            #[inline]
            pub const unsafe fn build(self) -> #target < #ty_generics > {
                let Self { inner } = self;
                unsafe {
                    // SAFETY: caller promises that all fields are initialized
                    ::core::mem::MaybeUninit::assume_init(inner)
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
    let EmitContext {
        unchecked_builder,
        ty_generics,
        fields,
        packed,
        ..
    } = ctx;

    let mut output = TokenStream::new();

    let write_ident = if *packed {
        format_ident!("write_unaligned")
    } else {
        format_ident!("write")
    };

    for FieldInfo {
        ident,
        name,
        ty,
        vis,
        doc,
        ..
    } in *fields
    {
        output.extend(quote::quote! {
            #(#[doc = #doc])*
            #[inline]
            #vis const fn #name(mut self, value: #ty) -> #unchecked_builder < #ty_generics >
            where
                #ty: ::core::marker::Sized,
            {
                unsafe {
                    // SAFETY: address is in bounds
                    // when we return, the generics assert that the field is initialized
                    // if `repr(packed)`, we use an unaligned write
                    ::core::ptr::#write_ident(
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

fn push_error(res: &mut syn::Result<()>, new: syn::Error) {
    match res {
        Ok(()) => *res = Err(new),
        Err(err) => err.combine(new),
    }
}

fn load_builder_name(target: &Ident, rename: Option<Ident>) -> Ident {
    rename.unwrap_or_else(|| format_ident!("{}Builder", target))
}

fn load_builder_fn_name(rename: Option<BoolOr<Ident>>) -> Option<Ident> {
    match rename {
        None | Some(BoolOr::Bool(true)) => Some(format_ident!("builder")),
        Some(BoolOr::Bool(false)) => None,
        Some(BoolOr::Value(ident)) => Some(ident),
    }
}

fn load_unchecked_builder_name(target: &Ident, rename: Option<Ident>) -> Ident {
    rename.unwrap_or_else(|| format_ident!("{}UncheckedBuilder", target))
}

fn load_where_clause(
    target: &Ident,
    ty_generics: TypeGenerics<'_>,
    where_clause: Option<WhereClause>,
) -> WhereClause {
    let mut where_clause = where_clause.unwrap_or_else(|| syn::parse_quote!(where));
    let self_clause = syn::parse_quote!(#target < #ty_generics >: ::core::marker::Sized);
    where_clause.predicates.push(self_clause);
    where_clause
}

fn load_fields(
    target: &Ident,
    builder_attrs: &BuilderAttrs,
    raw_fields: impl IntoIterator<Item = Field>,
) -> (Vec<FieldInfo>, syn::Result<()>) {
    let mut errors = Ok(());
    let mut fields = Vec::new();

    let mut unsized_tail = None::<Span>;

    for raw_field in raw_fields {
        if let Some(unsized_tail) = unsized_tail {
            push_error(
                &mut errors,
                syn::Error::new(
                    unsized_tail,
                    "`#[builder(unsized_tail)]` must be specified on the last field only",
                ),
            );
        }

        let attrs = match FieldAttrs::from_attributes(&raw_field.attrs) {
            Ok(attrs) => attrs,
            Err(err) => {
                push_error(&mut errors, err.into());
                FieldAttrs::default()
            },
        };

        if attrs.default.is_none() && builder_attrs.default.is_present() {
            push_error(
                &mut errors,
                syn::Error::new_spanned(
                    &raw_field,
                    "structs with `#[builder(default)]` must provide a default value for all fields",
                ),
            );
        }

        let ident = raw_field.ident.expect("must be a named field here");
        let name = attrs.rename.unwrap_or_else(|| ident.clone());
        let drop_flag = format_ident!("drop_{ident}");
        let gen_name = attrs.rename_generic.unwrap_or_else(|| {
            format_ident!(
                "_{}",
                ident.unraw().to_string().to_uppercase(),
                span = ident.span()
            )
        });

        let mut doc = get_doc(&raw_field.attrs);
        let doc_header = match &attrs.default {
            None => format!("Sets the [`{target}::{ident}`] field.\n"),
            Some(_) => {
                format!("Sets the [`{target}::{ident}`] field, replacing the default value.\n")
            },
        };

        doc.insert(0, syn::parse_quote!(#doc_header));

        fields.push(FieldInfo {
            ident,
            name,
            gen_name,
            drop_flag,
            ty: raw_field.ty,
            default: attrs.default,
            vis: attrs
                .vis
                .unwrap_or(Visibility::Public(<Token![pub]>::default())),
            doc,
            leak_on_drop: attrs.leak_on_drop.is_present(),
            unsized_tail: attrs.unsized_tail.is_some(),
        });

        unsized_tail = attrs.unsized_tail.map(|s| s.span());
    }

    (fields, errors)
}
