use darling::util::{Flag, SpannedValue};
use darling::{FromAttributes, FromDeriveInput, FromMeta};
use syn::{Expr, ExprLit, Ident, Lit, LitBool, Pat, PatType, ReturnType, Type, Visibility};

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(builder))]
pub struct BuilderAttrs {
    // darling recognizes `vis` as matching the struct visibility which i don't want
    #[darling(rename = "vis")]
    pub m_vis: Option<Visibility>,
    pub rename: Option<Ident>,
    pub rename_fn: Option<BoolOr<Ident>>,
    #[darling(default)]
    pub unchecked: BuilderUncheckedAttrs,
    pub default: Flag,
}

#[derive(Debug, FromDeriveInput)]
#[darling(attributes(repr), allow_unknown_fields)]
pub struct ReprAttrs {
    pub packed: Flag,
}

#[derive(Default, Debug, FromMeta)]
pub struct BuilderUncheckedAttrs {
    pub vis: Option<Visibility>,
    pub rename: Option<Ident>,
}

#[derive(Default, Debug, FromAttributes)]
#[darling(attributes(builder))]
pub struct FieldAttrs {
    pub rename: Option<Ident>,
    pub rename_generic: Option<Ident>,
    pub default: Option<Expr>,
    pub vis: Option<Visibility>,
    pub leak_on_drop: Flag,
    pub unsized_tail: Option<SpannedValue<Flag>>,
    #[darling(default)]
    pub setter: FieldSetterRaw,
}

pub struct FieldInfo {
    pub ident: Ident,
    pub name: Ident,
    pub gen_name: Ident,
    pub drop_flag: Ident,
    pub ty: Type,
    pub default: Option<Expr>,
    pub vis: Visibility,
    pub doc: Vec<Expr>,
    pub leak_on_drop: bool,
    pub unsized_tail: bool,
    pub setter: FieldSetter,
}

pub trait FieldInfoSliceExt {
    fn gen_names(&self) -> impl Iterator<Item = &Ident>;
}

impl FieldInfoSliceExt for [FieldInfo] {
    fn gen_names(&self) -> impl Iterator<Item = &Ident> {
        self.iter().map(|t| &t.gen_name)
    }
}

#[derive(Debug)]
pub enum BoolOr<T> {
    Bool(bool),
    Value(T),
}

impl<T: FromMeta> FromMeta for BoolOr<T> {
    fn from_expr(expr: &Expr) -> darling::Result<Self> {
        if let Expr::Lit(ExprLit {
            lit: Lit::Bool(LitBool { value, .. }),
            ..
        }) = *expr
        {
            return Ok(BoolOr::Bool(value));
        }

        T::from_expr(expr).map(BoolOr::Value)
    }
}

#[derive(Default, Debug, FromMeta)]
pub struct FieldSetterRaw {
    pub strip_option: Flag,
    pub transform: Option<Expr>,
}

#[derive(Default, Debug)]
pub enum FieldSetter {
    #[default]
    Default,
    StripOption,
    Transform(FieldTransform),
}

#[derive(Debug)]
pub struct FieldTransform {
    pub inputs: Vec<PatType>,
    pub body: Box<Expr>,
}

impl TryFrom<Expr> for FieldTransform {
    type Error = syn::Error;

    fn try_from(value: Expr) -> Result<Self, syn::Error> {
        let Expr::Closure(value) = value else {
            return Err(syn::Error::new_spanned(value, "expected closure"));
        };

        if !value.attrs.is_empty()
            || value.lifetimes.is_some()
            || value.constness.is_some()
            || value.movability.is_some()
            || value.asyncness.is_some()
            || value.capture.is_some()
            || !matches!(value.output, ReturnType::Default)
        {
            return Err(syn::Error::new_spanned(
                value,
                "closure must not have attributes, modifiers, or return type",
            ));
        }

        let inputs = value
            .inputs
            .into_iter()
            .map(|pat| match pat {
                Pat::Type(pat_type) => Ok(pat_type),
                _ => Err(syn::Error::new_spanned(
                    pat,
                    "closure inputs must all have an explicit type",
                )),
            })
            .collect::<Result<_, _>>()?;

        Ok(FieldTransform {
            inputs,
            body: value.body,
        })
    }
}
