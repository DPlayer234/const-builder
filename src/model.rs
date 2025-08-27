use darling::util::Flag;
use darling::{FromAttributes, FromDeriveInput, FromMeta};
use proc_macro2::TokenStream;
use syn::{Expr, ExprLit, Ident, Lit, LitBool, PatType, Type, Visibility};

#[derive(Default, Debug, FromDeriveInput)]
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

#[derive(Default, Debug, FromDeriveInput)]
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
    pub default: Option<Box<Expr>>,
    pub vis: Option<Visibility>,
    pub leak_on_drop: Flag,
    pub unsized_tail: Flag,
    #[darling(default)]
    pub setter: FieldSetterRaw,
}

pub struct FieldInfo<'a> {
    pub ident: &'a Ident,
    pub name: Ident,
    pub gen_name: Ident,
    pub drop_flag: Ident,
    pub ty: &'a Type,
    pub default: Option<Box<Expr>>,
    pub vis: Visibility,
    pub doc: Vec<Expr>,
    pub leak_on_drop: bool,
    pub unsized_tail: bool,
    pub setter: FieldSetter,
}

pub trait FieldInfoSliceExt {
    fn gen_names(&self) -> impl Iterator<Item = &Ident>;
}

impl FieldInfoSliceExt for [FieldInfo<'_>] {
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
    pub lifetimes: TokenStream,
    pub inputs: Vec<PatType>,
    pub body: Box<Expr>,
}
