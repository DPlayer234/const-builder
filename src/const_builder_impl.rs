use std::borrow::Cow;
use std::slice;

use darling::util::Flag;
use darling::{Error, FromAttributes as _, FromDeriveInput as _};
use proc_macro2::{Span, TokenStream};
use quote::format_ident;
use syn::ext::IdentExt as _;
use syn::spanned::Spanned as _;
use syn::{Data, Fields, FieldsNamed, Ident, Token, Type, Visibility, WhereClause};

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
    fields: &'a [FieldInfo<'a>],
    packed: bool,
}

pub fn entry_point(input: syn::DeriveInput) -> TokenStream {
    // accumulate all errors here whenever possible. this allows us to both emit as
    // much as correct code as possible while also eagerly emitting every error in
    // the input, providing better diagnostics for the user, and at least allowing
    // partial intellisense for the parts that we could generate.
    let mut acc = Error::accumulator();

    let builder_attrs = acc
        .handle(BuilderAttrs::from_derive_input(&input))
        .unwrap_or_default();

    let repr_attrs = acc
        .handle(ReprAttrs::from_derive_input(&input))
        .unwrap_or_default();

    // if we are dealing with wrong kind of item, no reason to continue, just error
    // out. we only continue for structs with named fields.
    let Some(raw_fields) = acc.handle(find_named_fields(&input.data)) else {
        return into_write_errors(acc);
    };

    let ty_generics = TypeGenerics(&input.generics.params);
    let impl_generics = ImplGenerics(&input.generics.params);

    let fields = load_fields(&input.ident, &builder_attrs, raw_fields, &mut acc);
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
        packed: repr_attrs.packed.is_some(),
    };

    let mut output = emit_main(&ctx);
    output.extend(emit_drop(&ctx));
    output.extend(emit_fields(&ctx));
    output.extend(emit_builder_fn(&ctx));

    if builder_attrs.default.is_present() {
        output.extend(emit_default(&ctx));
    }

    output.extend(emit_unchecked(&ctx));

    if let Err(err) = acc.finish() {
        output.extend(err.write_errors());
    }

    output
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

    let t_true = format_ident!("true");
    let build_args = fields.iter().map(|f| {
        if f.default.is_some() {
            &f.gen_name
        } else {
            &t_true
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
        impl < #impl_generics #( const #field_generics1: ::core::primitive::bool ),* > Drop for #builder < #ty_generics #(#field_generics2),* > #where_clause {
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
        .iter()
        .filter(|f| !f.leak_on_drop && if *packed { !f.unsized_tail } else { true });

    let field_count = dropped_fields.clone().count();
    let mut field_vars = dropped_fields.clone().map(|f| &f.drop_flag);
    let mut field_var_generics = dropped_fields.map(|f| &f.gen_name);

    let pack_count = usize::div_ceil(field_count, pack_size);
    let packed_idents = (0..pack_count)
        .map(|i| format_ident!("packed_{i}"))
        .collect::<Vec<_>>();

    let mut pack = TokenStream::new();
    let mut unpack = TokenStream::new();

    for pack_ident in &packed_idents {
        let field_vars = field_vars.by_ref().take(pack_size);
        let field_var_generics = field_var_generics.by_ref().take(pack_size);

        let mask1 = (0usize..).map(|i| 1u32 << i);
        let mask2 = mask1.clone();

        pack.extend(quote::quote! {
            (0 #( | if #field_var_generics { #mask1 } else { 0 } )*),
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
            // force const-eval to reduce debug binary size
            if const { ::core::mem::needs_drop::<#ty>() } && #drop_flag {
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
        let message = format!("packed struct unsized tail field `{ident}` cannot be dropped");

        // this span puts the error message on the field type instead of the macro
        quote::quote_spanned! {ty.span()=>
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

    let t_true = format_ident!("true");
    let t_false = format_ident!("false");

    for (
        index,
        FieldInfo {
            ident,
            name,
            ty,
            vis,
            doc,
            setter,
            ..
        },
    ) in fields.iter().enumerate()
    {
        let set_generics = fields
            .iter()
            .enumerate()
            .filter_map(|(i, f)| (i != index).then_some(&f.gen_name));

        let set_args = fields
            .iter()
            .enumerate()
            .map(|(i, f)| if i == index { &t_false } else { &f.gen_name });

        let post_set_args = fields
            .iter()
            .enumerate()
            .map(|(i, f)| if i == index { &t_true } else { &f.gen_name });

        let mut ty = *ty;
        let (inputs, cast, tys, life) = split_setter(setter, &mut ty);

        output.extend(quote::quote_spanned! {ident.span()=>
            impl < #impl_generics #( const #set_generics: ::core::primitive::bool ),* > #builder < #ty_generics #(#set_args),* > #where_clause {
                #(#[doc = #doc])*
                #[inline]
                // may occur with `transform` that specifies the same input ty for multiple parameters
                #[allow(clippy::type_repetition_in_bounds, clippy::multiple_bound_locations)]
                #vis const fn #name #life (self, #inputs) -> #builder < #ty_generics #(#post_set_args),* >
                where
                    #(#tys: ::core::marker::Sized,)*
                {
                    #cast
                    // SAFETY: same fields considered initialized, except `#name`,
                    // which is now considered initialized and actually initialized.
                    unsafe { self.into_unchecked().#name(value).assert_init() }
                }
            }
        });
    }

    output
}

// double-ref `ty` so we can return a slice without allocating for the common
// case and avoid cloning `Type` values for the transform cases that allocate a
// `Vec` of references. the outer ref is mutable so we can use it to store a ref
// to the inner `Option` type for the `strip_option` case.
fn split_setter<'t>(
    setter: &'t FieldSetter,
    ty: &'t mut &'t Type,
) -> (
    TokenStream,             // inputs
    TokenStream,             // cast
    Cow<'t, [&'t Type]>,     // tys
    Option<&'t TokenStream>, // life
) {
    match setter {
        FieldSetter::Default => (
            quote::quote! { value: #ty },
            TokenStream::new(),
            slice::from_ref(ty).into(),
            None,
        ),
        FieldSetter::StripOption => {
            *ty = first_generic_arg(ty).unwrap_or(ty);
            (
                quote::quote! { value: #ty },
                quote::quote! { let value = ::core::option::Option::Some(value); },
                slice::from_ref(ty).into(),
                None,
            )
        },
        FieldSetter::Transform(transform) => {
            let inputs = &transform.inputs;
            let body = &transform.body;
            (
                quote::quote! { #(#inputs),* },
                quote::quote! { let value = #body; },
                transform.inputs.iter().map(|t| &*t.ty).collect(),
                transform.lifetimes.as_ref(),
            )
        },
    }
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
    let field_default_values = fields.iter().filter_map(|f| f.default.as_deref());

    let field_setters = emit_unchecked_fields(ctx);
    let structure_check = emit_structure_check(ctx);

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

            /// Asserts that the fields specified by the const generics as well as all optional
            /// fields are initialized and promotes this value into a checked builder.
            ///
            /// # Safety
            ///
            /// The fields whose const generic are `true` and all optional fields have to be
            /// initialized.
            ///
            /// Optional fields are initialized by [`Self::new`] by default, however using
            /// [`Self::as_uninit`] allows de-initializing them. This means that this function
            /// isn't even necessarily safe to call if all const generics are `false`.
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
                #structure_check

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
        target,
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
        let align_check = if *packed {
            TokenStream::new()
        } else {
            // potential post-mono error that trips when UB may happen.
            // see more in the comment above `__assert_field_aligned` below.
            quote::quote! {
                const {
                    Self::__assert_field_aligned(
                        ::core::mem::offset_of!(#target < #ty_generics >, #ident),
                        ::core::mem::align_of::<#ty>(),
                    );
                }
            }
        };

        output.extend(quote::quote_spanned! {ident.span()=>
            #(#[doc = #doc])*
            #[inline]
            // may trigger when field names begin with underscores
            #[allow(clippy::used_underscore_binding)]
            #vis const fn #name(mut self, value: #ty) -> #unchecked_builder < #ty_generics >
            where
                #ty: ::core::marker::Sized,
            {
                #align_check
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

    if !*packed {
        // note: the goal here is to check that no other proc macro attribute added
        // `repr(packed)` in such a way that we didn't get to see it. emitting the
        // non-packed code for a packed struct would lead to UB.
        // the inverse, i.e. emitting packed code for a non-packed struct, however is
        // fine. that only adds a few restrictions and unaligned writes, so at worst
        // it's suboptimal, but still correct.
        output.extend(quote::quote! {
            /// Internal function to assert that fields are sufficiently aligned.
            ///
            /// A failure implies that the struct is `#[repr(packed)]` even though
            /// the macro did not see that attribute.
            #[doc(hidden)]
            const fn __assert_field_aligned(field_offset: ::core::primitive::usize, field_ty_align: ::core::primitive::usize) {
                let struct_align = ::core::mem::align_of::<#target < #ty_generics >>();
                let aligned_to_struct = (struct_align % field_ty_align) == 0;
                let aligned_to_offset = (field_offset % field_ty_align) == 0;
                ::core::assert!(
                    aligned_to_struct && aligned_to_offset,
                    "struct appears packed despite no visible repr attribute"
                );
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
        ..
    } = ctx;

    let field_idents1 = fields.iter().map(|f| f.ident);
    let field_idents2 = field_idents1.clone();
    let field_tys1 = fields.iter().map(|f| f.ty);
    let field_tys2 = field_tys1.clone();

    quote::quote! {
        // statically validate that the macro-seen fields match the final struct
        #[doc(hidden)]
        #[allow(
            clippy::too_many_arguments,
            clippy::multiple_bound_locations,
            clippy::type_repetition_in_bounds,
            clippy::used_underscore_binding,
        )]
        fn _derive_includes_every_field < #impl_generics > ( #( #field_idents1: #field_tys1 ),* ) -> #target < #ty_generics >
        #where_clause, #(#field_tys2: ::core::marker::Sized),*
        {
            #target { #(#field_idents2),* }
        }
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
    let mut where_clause = where_clause.unwrap_or_else(empty_where_clause);
    let self_clause = syn::parse_quote!(#target < #ty_generics >: ::core::marker::Sized);
    where_clause.predicates.push(self_clause);
    where_clause
}

fn find_named_fields(data: &Data) -> darling::Result<&FieldsNamed> {
    if let Data::Struct(data) = data {
        if let Fields::Named(raw_fields) = &data.fields {
            return Ok(raw_fields);
        }
    }

    // just keep the call site span (i.e. the derive itself)
    Err(Error::custom(
        "`ConstBuilder` can only be derived for structs with named fields",
    ))
}

// this function accumulates errors so the field setters can be emitted for
// intellisense by the caller even when there are problems.
fn load_fields<'f>(
    target: &Ident,
    builder_attrs: &BuilderAttrs,
    raw_fields: &'f FieldsNamed,
    acc: &mut darling::error::Accumulator,
) -> Vec<FieldInfo<'f>> {
    let mut fields = Vec::with_capacity(raw_fields.named.len());
    let mut unsized_tail = Flag::default();

    for pair in raw_fields.named.pairs() {
        let raw_field = pair.into_value();
        if unsized_tail.is_present() {
            let err = Error::custom(
                "`#[builder(unsized_tail)]` must be specified on the last field only",
            );
            acc.push(err.with_span(&unsized_tail.span()));
        }

        let ident = raw_field
            .ident
            .as_ref()
            .expect("must be a named field here");

        let attrs = acc
            .handle(FieldAttrs::from_attributes(&raw_field.attrs))
            .unwrap_or_default();

        if attrs.default.is_none() && builder_attrs.default.is_present() {
            let err = Error::custom(
                "structs with `#[builder(default)]` must provide a default value for all fields",
            );
            acc.push(err.with_span(ident));
        }

        let name = attrs.rename.unwrap_or_else(|| ident.clone());

        // ensure correct ident formatting. overriding the span gets rid of a variable
        // name warning, probably because the span no longer points at the field.
        let drop_flag = format_ident!("drop_{}", ident, span = Span::call_site());

        let gen_name = attrs.rename_generic.unwrap_or_else(|| {
            format_ident!(
                "_{}",
                ident.unraw().to_string().to_uppercase(),
                span = ident.span()
            )
        });

        let doc_header = match &attrs.default {
            None => format!("Sets the [`{target}::{ident}`] field."),
            Some(_) => {
                format!("Sets the [`{target}::{ident}`] field, replacing the default value.")
            },
        };

        // need the empty line as a separate entry so rustdoc splits the paragraphs
        let mut doc = Vec::new();
        doc.push(Cow::Owned(lit_str_expr(&doc_header)));
        doc.push(Cow::Owned(lit_str_expr("")));
        doc.extend(iter_doc_exprs(&raw_field.attrs).map(Cow::Borrowed));

        if attrs.setter.strip_option.is_present() && attrs.setter.transform.is_some() {
            let err = Error::custom(
                "may only specify one of the following `setter` fields: `strip_option`, `transform`",
            );
            acc.push(err.with_span(&attrs.setter.strip_option.span()));
        }

        if attrs.setter.strip_option.is_present() && first_generic_arg(&raw_field.ty).is_none() {
            // best-effort type guessing and error message. if we get here, the emitted code
            // will fail to compile anyways, so this is just here to give slightly better
            // errors for some cases. note that this doesn't catch every case, f.e. if the
            // type is `PhantomData<u32>`, it will look fine here but error later, and due
            // to aliases, we can't really do much better.
            let err = Error::custom(
                "cannot determine element type for `strip_option`, use `Option<_>` directly",
            );
            acc.push(err.with_span(&attrs.setter.strip_option.span()));
        }

        let setter = if attrs.setter.strip_option.is_present() {
            FieldSetter::StripOption
        } else if let Some(transform) = attrs.setter.transform {
            FieldSetter::Transform(to_field_transform(transform, acc))
        } else {
            FieldSetter::Default
        };

        fields.push(FieldInfo {
            ident,
            name,
            gen_name,
            drop_flag,
            ty: &raw_field.ty,
            default: attrs.default,
            vis: attrs
                .vis
                .unwrap_or(Visibility::Public(<Token![pub]>::default())),
            doc,
            leak_on_drop: attrs.leak_on_drop.is_present(),
            unsized_tail: attrs.unsized_tail.is_present(),
            setter,
        });

        unsized_tail = attrs.unsized_tail;
    }

    fields
}
