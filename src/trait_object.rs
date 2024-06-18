//! Generates the owned trait object struct. Not to be confused with the representation struct.

use proc_macro2::TokenStream;
use quote::{format_ident, quote, ToTokens};
use syn::{punctuated::Punctuated, token, Attribute, FnArg, Path, Visibility, Type};

use crate::{attr::StageStash, marker_traits::MarkerTrait, vtable::VtableItem};

pub fn generate_trait_object<'a>(
    stash: &mut StageStash,
    visibility: Visibility,
    inline_vtable: bool,
    has_static_bound: bool,
    attributes: impl IntoIterator<Item = &'a Attribute> + Clone,
    markers: impl IntoIterator<Item = &'a MarkerTrait>,
) -> syn::Result<TokenStream> {
    let StageStash {
        trait_name,
        repr_name,
        vtable_name,
        trait_object_name,
        vtable_items,
        ..
    } = stash;
    let trait_object_name_as_path = trait_object_name.clone().into();
    #[derive(Copy, Clone)]
    struct MarkerToImpl<'a> {
        marker_trait: &'a MarkerTrait,
        implementor: &'a Path,
        elided_lifetime: bool,
    }
    impl<'a> ToTokens for MarkerToImpl<'a> {
        fn to_tokens(&self, token_stream: &mut TokenStream) {
            token_stream.extend((*self).into_token_stream());
        }
        fn into_token_stream(self) -> TokenStream {
            let implementor = self.implementor;
            let implementor = if self.elided_lifetime {
                quote! {#implementor <'_>}
            } else {
                quote! {implementor} // Performance is my passion
            };
            self.marker_trait.as_impl_for(&implementor)
        }
    }
    struct VtableItemToImplThunk(VtableItem);
    impl ToTokens for VtableItemToImplThunk {
        fn to_tokens(&self, token_stream: &mut TokenStream) {
            let signature = self
                .0
                .clone()
                .into_signature(|x| format_ident!("__arg{}", x));
            let call_args = signature
                .inputs
                .clone()
                .into_iter()
                .map(|param| match param {
                    FnArg::Typed(param) => param.pat.into_token_stream(),
                    FnArg::Receiver(..) => quote! {
                        self.0.as_ptr() as *mut _
                    },
                })
                .collect::<Punctuated<_, token::Comma>>();
            let call_name = signature.ident.clone();
            (quote! {
                #signature {
                    unsafe {
                        ((self.vtable()).#call_name)(#call_args)
                    }
                }
            })
            .to_tokens(token_stream);
        }
    }

    attributes
        .clone()
        .into_iter()
        .try_for_each(check_attribute)?;
    let attributes = attributes.into_iter();
    let marker_impls = markers.into_iter().map(|marker_trait| MarkerToImpl {
        marker_trait,
        implementor: &trait_object_name_as_path,
        elided_lifetime: !has_static_bound,
    });

    let impl_thunks = vtable_items.iter().cloned().map(VtableItemToImplThunk);
    let (phantomdata, generics, creation_bound, impl_elided_lifetime) = if has_static_bound {
        let phantomdata = quote! {
            ::core::marker::PhantomData<&'static ()>
        };
        // Those three are empty, so use the tuple Default impl to write this concisely
        let (generics, creation_bound, impl_elided_lifetime) = Default::default();
        (phantomdata, generics, creation_bound, impl_elided_lifetime)
    } else {
        let phantomdata = quote! {
            ::core::marker::PhantomData<&'inner ()>
        };
        let generics = quote! { <'inner> };
        let creation_bound = quote! { 'inner };
        let impl_elided_lifetime = quote! { <'_> };
        (phantomdata, generics, creation_bound, impl_elided_lifetime)
    };
    let vtable_getter_impl = {
        let vtable_pointer_cast = if inline_vtable {
            quote! { as *mut }
        } else {
            quote! { as *mut &'static }
        };
        quote! {
            unsafe { &*(self.0.as_ptr() #vtable_pointer_cast #vtable_name) }
        }
    };
    let result = quote! {
        #(#attributes)*
        #[repr(transparent)]
        #visibility struct #trait_object_name #generics (
            ::core::ptr::NonNull<#vtable_name>,
            #phantomdata,
        );
        impl #generics #trait_object_name #generics {
            /// Constructs a boxed thin trait object from a type implementing the trait.
            #[inline]
            pub fn new<
                T: #trait_name + Sized + #creation_bound
                >(val: T) -> Self {
                    unsafe { Self::from_raw(#repr_name::__thintraitobjectmacro_repr_create(val) as *mut _) }
            }
            /// Creates a thin trait object directly from a raw pointer to its vtable.
            ///
            /// # Safety
            /// This constructor, by its nature, is hugely unsafe and should be avoided when possible. The following invariants must be upheld:
            /// - The pointer must not be null and must point to a valid thin trait object as expected by its vtable which is not uninitialized;
            /// - The function pointers in the vtable must not be null and must point to valid functions with correct ABI and signature;
            /// - The function pointers must have the same safety contract as implied and not a stronger one: only cause UB if the vtable pointer passed to them is invalid or, if those are unsafe in the trait itself, cause UB if the safety contract in their declarations is violated;
            /// - If the trait is unsafe, the function pointers must follow the trait's contract for valid implementations;
            /// - The pointer was not returned by [`as_raw`] which was called on an object which was not put into [`ManuallyDrop`] or consumed by [`mem::forget`], otherwise undefined behavior will be invoked when both are dropped.
            ///
            /// [`as_raw`]: #method.as_raw " "
            /// [`ManuallyDrop`]: https://doc.rust-lang.org/std/mem/struct.ManuallyDrop.html " "
            /// [`mem::forget`]: https://doc.rust-lang.org/std/mem/fn.forget.html " "
            #[inline]
            pub const unsafe fn from_raw(ptr: *mut ()) -> Self {
                // `new_unchecked` is fine because that's part of the safety
                // contract and the entire thing is horribly unsafe anyway.
                Self(::core::ptr::NonNull::new_unchecked(ptr as *mut _), ::core::marker::PhantomData)
            }
            /// Extracts the contained pointer to the trait object.
            ///
            /// Unlike [`into_raw`], ownership of the pointer is not released, and as such will be dropped normally. Unless the original copy is removed via [`mem::forget`] or [`ManuallyDrop`], calling [`from_raw`] and then dropping will cause undefined behavior.
            ///
            /// [`into_raw`]: #method.into_raw " "
            /// [`from_raw`]: #method.from_raw " "
            /// [`ManuallyDrop`]: https://doc.rust-lang.org/std/mem/struct.ManuallyDrop.html " "
            /// [`mem::forget`]: https://doc.rust-lang.org/std/mem/fn.forget.html " "
            #[inline]
            pub const fn as_raw(&self) -> *mut () {
                self.0.as_ptr() as *mut ()
            }
            /// Releases ownership of the trait object, returning the contained pointer. It is the caller's responsibility to drop the trait object at a later time using [`from_raw`].
            ///
            /// For a version which does not release ownership, see [`as_raw`].
            ///
            /// [`from_raw`]: #method.from_raw " "
            /// [`as_raw`]: #method.as_raw " "
            #[inline]
            pub fn into_raw(self) -> *mut () {
                let pointer = self.as_raw();
                ::core::mem::forget(self);
                pointer
            }
            /// Retrieves the raw vtable of the contained trait object.
            pub fn vtable(&self) -> &#vtable_name {
                #vtable_getter_impl
            }
        }
        #[allow(clippy::ref_in_deref)] // see https://github.com/rust-lang/rust-clippy/issues/6658
        impl #trait_name for #trait_object_name #impl_elided_lifetime {
            #(#impl_thunks)*
        }
        impl ::core::ops::Drop for #trait_object_name #impl_elided_lifetime {
            fn drop(&mut self) {
                unsafe { (self.vtable().drop)(self.0.as_ptr() as *mut ::core::ffi::c_void) }
            }
        }
        #(#marker_impls)*
    };
    Ok(result)
}








pub fn generate_dotnet_wrapper_objects_for_trait<'a>(
    stash: &mut StageStash,
) -> syn::Result<TokenStream> {
    let StageStash {
        vtable_items,
        trait_name,
        trait_object_name,
        ..
    } = stash;

    struct VtableItemToImplThunk(VtableItem, String,String);
    impl ToTokens for VtableItemToImplThunk {
        fn to_tokens(&self, token_stream: &mut TokenStream) {
            let signature = self.0.clone().into_signature(|x| format_ident!("__arg{}", x));
            let trait_object_name = format_ident!("{}", self.1);
            let trait_name = format_ident!("{}", self.2);


            let call_name = signature.ident.clone();
            let func_name = format_ident!("{}_{}", trait_name, call_name);


            let func_args = signature.inputs.iter().map(|arg| match arg {
                FnArg::Typed(pat_type) => {
                    let pat = &pat_type.pat;
                    let ty = &pat_type.ty;
                    if is_primitive(ty) {
                        quote! { #pat: #ty }
                    } else {
                        quote! { #pat: *const std::os::raw::c_char }
                    }
                },
                FnArg::Receiver(_) => quote! { instance_ptr: *mut std::ffi::c_void },
            });

            let call_args = signature
                .inputs
                .clone()
                .into_iter()
                .filter_map(|param| match param {
                    FnArg::Typed(pat_type) => {
                        let pat = &pat_type.pat;
                        let ty = &pat_type.ty;
                        if is_primitive(ty) {
                            Some(quote! {
                                {
                                    let r1 = #pat;
                                    log::info!("Primitive Param: {} {:?}",stringify!(#pat), r1);
                                    r1
                                }
                            })
                        }
                        else if is_string(ty) {
                            Some(quote! {
                            unsafe {
                                let c_str = std::ffi::CStr::from_ptr(#pat);
                                let string_unwrapped = c_str.to_str().expect("Failed to get string from c string");
                                let r1 = string_unwrapped.to_string();
                                //log::info!("String Param: {} {:?}",stringify!(#pat), r1);
                                r1
                            }
                        })
                        } else {
                            Some(quote! {
                            unsafe {
                                let c_str = std::ffi::CStr::from_ptr(#pat);
                                let string_unwrapped = c_str.to_str().expect("Failed to get string from c string");
                                let r1 = serde_json::from_str(&string_unwrapped).expect(format!("Failed to serialize param {} from string {}", stringify!(#pat), string_unwrapped).as_str());
                                //log::info!("Typed Param: {} {:?}",stringify!(#pat), r1);
                                r1
                            }
                        })
                        }
                    },
                    FnArg::Receiver(_) => None,
                })
                .collect::<Punctuated<_, token::Comma>>();

            let output_type = match &signature.output {
                syn::ReturnType::Default => quote! { *const std::os::raw::c_char  },
                syn::ReturnType::Type(_, ty) => {
                    if is_primitive(ty) {
                        quote! { #ty }
                    } else {
                        quote! { *const std::os::raw::c_char }
                    }
                }
            };

            let return_stmt = match &signature.output {
                syn::ReturnType::Default => quote! {return std::ptr::null()},
                syn::ReturnType::Type(_, ty) => {
                    if is_primitive(ty) {
                        quote! { result }
                    } else {
                        quote! {
                        log::info!("Unserialized Result: {:?}", result);
                        let result_str = serde_json::to_string(&result).expect("Failed to serialize result");
                        log::info!("Serialized Result: {}", result_str);
                        std::ffi::CString::new(result_str).expect("Failed to get typed result from string result").into_raw()
                    }
                    }
                }
            };

            (quote! {
            #[no_mangle]
            pub extern "C" fn #func_name(#(#func_args),*) -> #output_type {
                let mut obj = unsafe { #trait_object_name::from_raw(instance_ptr as *mut ()) };
                log::info!("Calling: {}", stringify!(#trait_object_name::#call_name));
                let panicresult = ::std::panic::catch_unwind(::std::panic::AssertUnwindSafe(|| {
                    obj.#call_name(#call_args)
                }));

                unsafe { obj.into_raw(); } // Stops pointer from being destroyed after each call

                match panicresult {
                    ::std::result::Result::Ok(result) => {
                        ::log::info!("Finished Calling: {}", ::std::stringify!(#trait_object_name::#call_name));
                        #return_stmt// Assuming `#return_stmt` uses the value returned from `#call_name`
                    }
                    ::std::result::Result::Err(err) => {
                        ::log::error!("Error calling {}: {:?}", ::std::stringify!(#trait_object_name::#call_name), err);
                        let result = format!("{{\"Err\" : \"{:?}\"}}", err);
                        #return_stmt
                    }
                }
            }
        }).to_tokens(token_stream);
        }
    }

    let impl_thunks = vtable_items.iter().cloned().map(|it | VtableItemToImplThunk(it, trait_object_name.to_string(), trait_name.to_string()));

    let result = quote! {
           #(#impl_thunks)*
    };
    Ok(result)
}

fn is_primitive(ty: &Box<Type>) -> bool {
    matches!(
        **ty,
        Type::Path(ref p) if [
            "i8", "i16", "i32", "i64", "i128",
            "u8", "u16", "u32", "u64", "u128",
            "f32", "f64",
            "bool"
        ].contains(&p.path.segments[0].ident.to_string().as_str())
    )
}
fn is_string(ty: &Box<Type>) -> bool {
    matches!(
        **ty,
        Type::Path(ref p) if [
            "String"
        ].contains(&p.path.segments[0].ident.to_string().as_str())
    )
}

fn check_attribute(attribute: &Attribute) -> syn::Result<()> {
    let name = &attribute.path;
    let ident = &name.segments[0].ident;
    let span = ident.span();
    let error = match ident.to_string().as_str() {
        "derive" => syn::Error::new(
            span,
            "\
cannot use derive macros on the thin trait object structure because some \
derived traits may cause undefined behavior when derived on it, such as `Copy`",
        ),
        "repr" => syn::Error::new(
            span,
            "\
the trait object structure already has a `#[repr(transparent)]` annotation",
        ),
        _ => return Ok(()),
    };
    Err(error)
}
