use std::mem::take;

use darling::FromAttributes;
use proc_macro2::TokenStream;
use quote::ToTokens as _;
use syn::parse::Parse;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned as _;
use syn::{
    AngleBracketedGenericArguments, Attribute, Generics, Ident, ImplItem, ItemImpl, PathArguments,
    ReturnType, Token, Type,
};

use crate::model::*;
use crate::util::*;

struct Args {
    generics: Punctuated<Ident, Token![,]>,
}

impl Parse for Args {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Args {
            generics: input.parse_terminated(Ident::parse, Token![,])?,
        })
    }
}

pub fn entry_point(args: TokenStream, mut impl_item: ItemImpl) -> syn::Result<TokenStream> {
    let args: Args = syn::parse2(args)?;

    let mut output = TokenStream::new();
    let mut errors = Ok(());

    let mut unused_items = Vec::new();
    for item in take(&mut impl_item.items) {
        match item {
            ImplItem::Fn(mut item_fn) => {
                let attrs = take_builder_attrs(&mut item_fn.attrs);
                if attrs.is_empty() {
                    unused_items.push(ImplItem::Fn(item_fn));
                    continue;
                }

                let attrs = push_result(&mut errors, SetsAttrs::from_attributes(&attrs));

                let set_indices: Vec<_> = args
                    .generics
                    .iter()
                    .enumerate()
                    .filter(|(_, path)| attrs.sets.iter().any(|s| s.is_ident(*path)))
                    .map(|(index, _)| index)
                    .collect();

                if set_indices.len() != attrs.sets.len() {
                    push_error(
                        &mut errors,
                        syn::Error::new(
                            attrs.sets.span(),
                            "`sets` has to be a subset of the parameters to `custom_impl`",
                        ),
                    );
                }

                let mut new_impl_item = impl_item.clone();

                push_result(
                    &mut errors,
                    pad_generics(
                        Some(&mut new_impl_item.generics),
                        &mut new_impl_item.self_ty,
                        &args,
                        Some((&set_indices, false)),
                    ),
                );

                if let ReturnType::Type(_, ty) = &mut item_fn.sig.output
                    && let Type::Path(path) = &**ty
                    && path.path.is_ident("Self")
                {
                    ty.clone_from(&impl_item.self_ty);
                    push_result(
                        &mut errors,
                        pad_generics(None, ty, &args, Some((&set_indices, true))),
                    );
                }

                new_impl_item.items.push(ImplItem::Fn(item_fn));
                output.extend(new_impl_item.into_token_stream());
            },
            item => unused_items.push(item),
        }
    }

    if !unused_items.is_empty() {
        push_result(
            &mut errors,
            pad_generics(
                Some(&mut impl_item.generics),
                &mut impl_item.self_ty,
                &args,
                None,
            ),
        );

        impl_item.items = unused_items;
        output.extend(impl_item.into_token_stream());
    }

    if let Err(err) = errors {
        output.extend(err.into_compile_error());
    }

    Ok(output)
}

fn take_builder_attrs(attrs: &mut Vec<Attribute>) -> Vec<Attribute> {
    let (l, r) = take(attrs)
        .into_iter()
        .partition(|i| i.meta.path().is_ident("builder"));
    *attrs = r;
    l
}

fn get_generics(ty: &mut Type) -> Option<&mut AngleBracketedGenericArguments> {
    let Type::Path(path) = ty else { return None };

    let last = path.path.segments.last_mut()?;
    if let PathArguments::None = &last.arguments {
        last.arguments = PathArguments::AngleBracketed(syn::parse_quote!(<>));
    }

    match &mut last.arguments {
        PathArguments::AngleBracketed(args) => Some(args),
        _ => None,
    }
}

fn pad_generics(
    mut generics: Option<&mut Generics>,
    ty: &mut Type,
    args: &Args,
    except: Option<(&[usize], bool)>,
) -> syn::Result<()> {
    let span = ty.span();
    let ty_args =
        get_generics(ty).ok_or_else(|| syn::Error::new(span, "invalid custom_impl type"))?;

    for (index, generic) in args.generics.iter().enumerate() {
        if let Some(generics) = generics.as_mut()
            && except.is_none_or(|(i, _)| !i.contains(&index))
        {
            generics
                .params
                .push(syn::parse_quote!( const #generic: ::core::primitive::bool ));
        }

        if let Some((i, bool)) = except
            && i.contains(&index)
        {
            ty_args.args.push(syn::parse_quote!(#bool));
        } else {
            ty_args.args.push(syn::parse_quote!(#generic));
        }
    }

    Ok(())
}
