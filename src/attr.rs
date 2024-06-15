//! The main body of the attribute macro. Uses entirely `proc_macro2` stuff to make unit testing possible — compile error conversions and `proc_macro` conversions are delegated to the crate root wrapper.

use super::{marker_traits::*, options::*, repr::*, trait_object::*, vtable::*};
use proc_macro2::{Ident, Span, TokenStream};
use quote::{format_ident, quote};
use std::convert::TryFrom;
use syn::{
    parse::Parser,
    punctuated::Punctuated,
    Abi,
    Attribute,
    ItemTrait,
    Path,
    PathArguments,
    PathSegment,
    TraitBound,
    Visibility,
};

pub fn attribute_main(attr: TokenStream, item: TokenStream) -> Result<TokenStream, syn::Error> {
    let options = Punctuated::parse_terminated.parse2(attr)?;
    let config = Config::from(options);
    let trait_def = syn::parse2::<ItemTrait>(item)?;
    if !trait_def.generics.params.is_empty() {
        return Err(syn::Error::new_spanned(
            trait_def.generics.params,
            "generic traits are not yet supported by #[thin_trait_object]",
        ));
    }
    let vtable_name = config
        .vtable_name
        .unwrap_or_else(|| format_ident!("{}Vtable", &trait_def.ident));
    let trait_object_name = config
        .trait_object_name
        .unwrap_or_else(|| format_ident!("Boxed{}", &trait_def.ident));
    let vtable_items = trait_def
        .items
        .into_iter()
        .map(VtableItem::try_from)
        .collect::<Result<Vec<_>, _>>()?;
    let (markers, lifetime_bounds) = supertraits_to_markers_and_lifetimes(
        trait_def.supertraits,
        config.marker_traits.map_or(
            Box::new(default_marker_filter) as Box<dyn FnMut(_) -> _>,
            |markers| {
                Box::new(move |bound: TraitBound| {
                    for marker in &markers {
                        if bound.path == marker.path {
                            return Some((bound, marker.unsafety.is_some()));
                        }
                    }
                    None
                })
            },
        ),
    );
    let has_static_bound = lifetime_bounds
        .iter()
        .any(|lifetime| lifetime.ident == "static");
    let mut stash = StageStash {
        trait_name: trait_def.ident.clone(),
        vtable_name,
        trait_object_name,
        repr_name: repr_name_from_trait_name(trait_def.ident.clone()),
        vtable_items,
    };
    let trait_visibility = &trait_def.vis;
    let vtable = generate_vtable(
        &mut stash,
        config
            .vtable_visibility
            .unwrap_or_else(|| trait_visibility.clone()),
        config.vtable_attributes,
        config.drop_abi.as_ref(),
        config.store_layout,
    );
    let repr = generate_repr(
        &mut stash,
        config.inline_vtable,
        path_to_box(),
        config.drop_abi.as_ref(),
        config.store_layout,
    );
    let trait_object = generate_trait_object(
        &mut stash,
        config
            .trait_object_visibility
            .unwrap_or_else(|| trait_visibility.clone()),
        config.inline_vtable,
        has_static_bound,
        &config.trait_object_attributes,
        &markers,
    )?;
    let dot_net_wrappers = generate_dotnet_wrapper_objects_for_trait(
        &mut stash,
    )?;

    // We don't need to add the original input to the output here because the
    // public wrapper does that, see its definition for more on that.
    let output = quote! {
        #vtable #repr #trait_object #dot_net_wrappers
    };
    Ok(output)
}

struct Config {
    vtable_attributes: Vec<Attribute>,
    vtable_visibility: Option<Visibility>,
    vtable_name: Option<Ident>,
    inline_vtable: bool,
    trait_object_attributes: Vec<Attribute>,
    trait_object_visibility: Option<Visibility>,
    trait_object_name: Option<Ident>,
    drop_abi: Option<Abi>,
    marker_traits: Option<Vec<MarkerTrait>>,
    store_layout: bool,
}
impl From<AttrOptions> for Config {
    fn from(options: AttrOptions) -> Self {
        let mut config = Self::default();
        for option in options {
            match option {
                AttrOption::Vtable { additions, .. } => {
                    config.vtable_attributes = additions.attributes;
                    config.vtable_visibility = Some(additions.visibility);
                    config.vtable_name = Some(additions.name);
                }
                AttrOption::InlineVtable { val, .. } => {
                    config.inline_vtable = val.value;
                }
                AttrOption::TraitObject { additions, .. } => {
                    config.trait_object_attributes = additions.attributes;
                    config.trait_object_visibility = Some(additions.visibility);
                    config.trait_object_name = Some(additions.name);
                }
                AttrOption::DropAbi { abi, .. } => {
                    config.drop_abi = Some(Abi {
                        extern_token: Default::default(),
                        name: Some(abi),
                    })
                }
                AttrOption::MarkerTraits { marker_traits, .. } => {
                    config.marker_traits = Some(marker_traits.into_iter().collect());
                }
                AttrOption::StoreLayout { val, .. } => {
                    config.store_layout = val.value;
                }
            }
        }
        config
    }
}
// Not using the derive because at some point I'm gonna introduce a config entry which should have
// a different default and then forget to refator into a manual implementation, which will take
// some time to figure out when it becomes a bug.
impl Default for Config {
    fn default() -> Self {
        Self {
            vtable_attributes: Vec::new(),
            vtable_visibility: None,
            vtable_name: None,
            inline_vtable: false,
            trait_object_attributes: Vec::new(),
            trait_object_visibility: None,
            trait_object_name: None,
            drop_abi: None,
            marker_traits: None,
            store_layout: false,
        }
    }
}

/// A "shared stash" for the things that the various stages of the macro will store and use, used to clean up code and improve argument passing performance.
pub struct StageStash {
    pub trait_name: Ident,
    pub vtable_name: Ident,
    pub repr_name: Ident,
    pub trait_object_name: Ident,
    pub vtable_items: Vec<VtableItem>,
}

fn path_to_box() -> Path {
    let mut segments = Punctuated::new();
    let mut push_segment = |name| {
        segments.push(PathSegment {
            ident: Ident::new(name, Span::call_site()),
            arguments: PathArguments::None,
        });
    };

    #[cfg(feature = "std")]
    push_segment("std");
    #[cfg(not(feature = "std"))]
    push_segment("alloc");

    push_segment("boxed");
    push_segment("Box");

    Path {
        leading_colon: Some(Default::default()),
        segments,
    }
}
