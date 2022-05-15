use proc_macro::{TokenStream };
use syn::{NestedMeta, DeriveInput, Meta, Lit,  Field, TypePath, GenericArgument, parse_macro_input, DataStruct, Data, Fields, Error, spanned::Spanned, AngleBracketedGenericArguments, Type};
use quote::*;
use proc_macro2::{Ident, Span };

trait Repeat {
    fn can_repeat(&self) -> bool;
    fn repeat_type(&self) -> std::option::Option<Ident>;
    fn has_inner_attr(&self) -> bool;
    fn valid_inert_attr(&self) -> bool;
    fn get_inert_span(&self) -> std::option::Option<Span>;
}

impl Repeat for Field {
    fn can_repeat(&self) -> bool {
        for attr in &self.attrs {
            if attr.path.is_ident("builder") {
                return true;
            }
        }
        false
    }
    fn has_inner_attr(&self) -> bool {
        if &self.attrs.len() == &0 {
            return false;
        }
        true
    }
    fn get_inert_span(&self) -> std::option::Option<Span> {
        for attr in &self.attrs {
            if attr.path.is_ident("builder") {
                if let Ok(Meta::List(ml)) = attr.parse_meta() {
                    let nested = ml.nested.first();
                    if let Some(NestedMeta::Meta(Meta::NameValue(nv))) = nested {
                        if nv.path.is_ident("each"){
                            return None;
                        } else {
                            return Some(nv.path.span());
                        }
                    }
                }
            } else {
                return None;
            }
        }
        None
    }
    fn valid_inert_attr(&self) -> bool { 
        for attr in &self.attrs {
            if attr.path.is_ident("builder") {
                if let Ok(Meta::List(ml)) = attr.parse_meta() {
                    let nested = ml.nested.first();
                    if let Some(NestedMeta::Meta(Meta::NameValue(nv))) = nested {
                        if nv.path.is_ident("each"){
                            return true;
                        } else {
                            return false;
                        }
                    }
                }
            } else {
                return true;
            }
        }
        true
    }
    fn repeat_type(&self) -> Option<Ident> {
        for attr in &self.attrs {
           if attr.path.is_ident("builder") {
                if let Ok(Meta::List(ml)) = attr.parse_meta(){
                    let nested = ml.nested.first();
                    if let Some(NestedMeta::Meta(Meta::NameValue(nv))) = nested {
                        if let Lit::Str(lit) = &nv.lit {
                            let id = Ident::new(lit.value().as_ref(), Span::call_site());
                            return  Some(id);
                        }
                    }
                } 
            }
        }
        None
    }
}

trait Optional {
    fn is_optional(&self) -> bool;
    fn inner_type_child(&self) -> proc_macro2::TokenStream;
    fn inner_type(&self) -> Ident;
    fn get_ident(&self) -> Option<Ident>;
    fn is_option_ident(&self) -> bool;
}
impl Optional for Field {
    fn is_optional(&self) -> bool {
        self.inner_type() == "Option"
    }
    //This will get the type of a field
     fn inner_type(&self) -> Ident {
         let segments = match &self.ty { 
                    syn::Type::Path(syn::TypePath{ path, ..  }) =>  {
                    let path_seg = &path.segments;
                    let next = path_seg.into_iter().next().unwrap();
                    let ident = &next.ident;
                    ident.clone()
            }, _ => { let ide = Ident::new("NotUsed", Span::call_site()); ide } };
        segments
     }
     // This will get the inner type child e.g get String out of a Option<String>
    fn inner_type_child(&self) -> proc_macro2::TokenStream {
         let segments = match &self.ty { syn::Type::Path(syn::TypePath{ path, ..  }) if path.is_ident("Option")  =>  {
                     let path_seg = &path.segments;
                     let next = path_seg.into_iter().next().unwrap();
                     let arguments = &next.arguments;
                     let a = match arguments {
                        syn::PathArguments::AngleBracketed(
                            AngleBracketedGenericArguments{
                                args,
                                ..
                            }
                        ) => {
                            let args = args.into_iter().next().unwrap();
                            let ag = match args {
                                GenericArgument::Type(Type::Path(TypePath{ path, ..})) => {
                                    path.get_ident().to_token_stream()
                                },
                                _ => {
                                    self.ty.to_token_stream()
                                }
                            };

                            ag
                        },
                        _ =>  {
                                    self.ty.to_token_stream()
                        }
                     };   
                     a 
            },
            _ => {
                self.ty.to_token_stream()
            }
        };
        let seg = segments.clone();
        let res = &segments.into_iter().count();

        if *res > 1 {
            let s = seg.into_iter();
            let y = s.skip(2).next().into_token_stream();
            y
        } else {
            seg
        }
     }
    fn get_ident(&self) -> Option<Ident> {
       let result = match &self.ty {
            syn::Type::Path( syn::TypePath{ path, ..  },) =>  {
                    let id = path.get_ident();
                    id.cloned()
            },
             _ =>  {
                 let ide = Ident::new("calligraphy", Span::call_site());
                 Some(ide)
                }
                
       }; 
      result
    }

    fn is_option_ident(&self) -> bool {
        let result = match &self.ty {
            Type::Path(TypePath{ path, .. }) => path.is_ident("Option"),
            _ => false
        };
        result
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let i = parse_macro_input!(input as DeriveInput); 

    let span = i.span();
    //println!("{:#?}", i);
    let mut isp = Span::call_site();
    let named_fields = match  &i.data {
        Data::Struct(
            DataStruct {
                fields: Fields::Named(fields), ..
            }
        ) => {
            let named = &fields.named;
            let invalid_attr_present = named.iter().fold(false, |acc, f| {
                if f.has_inner_attr(){
                    if !f.valid_inert_attr(){
                            if let Some(sp) = f.get_inert_span(){
                                isp = sp;
                            }
                            acc || true
                    } else {
                        acc
                    }
                } else {
                    acc
                }
            });

            if invalid_attr_present {
                         return Error::new(isp, "`builder(each = \"...\")`")
                         .to_compile_error().into();
            }

            &fields.named
        },
       _ => {
                 return Error::new(span, "Builder not implemented for Enum or Unions")
                .to_compile_error().into();
       }
    }; 



    let fields = 
        named_fields.iter().map(|f| {
            let name  = &f.ident;
            let ty = &f.ty;
            if f.has_inner_attr(){
                if !f.valid_inert_attr(){
                    return Error::new(f.ident.span(), "Builder not implemented for Enum or Unions")
                    .to_compile_error().into();
                }
            }

            if f.is_optional() || f.can_repeat() {
                quote! { #name : #ty }
            } else {
                quote! { #name : std::option::Option<#ty> }
            }
        }); 
        
    let defaults = 
        named_fields.iter().map(|f| {
                let name = &f.ident;
                if f.can_repeat() {
                    let ty = &f.ty;
                    quote!{ #name: <#ty>::new() }
                } else {
                    quote! { #name: None }
                }
        }); 

    let setters =
        named_fields.iter().map(|f| {
            let name  = f.ident.as_ref().unwrap();
            let ty = &f.ty ;
            let ty1 = &f.inner_type_child();       

            if let Some(n) = f.repeat_type() {
                // The name of the setter must be checked against the builder each argument
                if name != &n {
                        quote!{
                            fn #n(&mut self, #name: #ty1) -> &mut Self {
                                self.#name.push(#name);
                                self
                            }
                        }
                } else {
                        quote!{
                            fn #name(&mut self, #name: #ty1) -> &mut Self {
                                self.#name.push(#name);
                                self
                            }
                        }
                }
            } else if f.is_optional() {
                quote!{
                    fn #name(&mut self, #name: #ty1) -> &mut Self {
                        self.#name = Some(#name);
                        self
                    }
                }
            } else {
                quote!{
                    fn #name(&mut self, #name: #ty) -> &mut Self {
                        self.#name = Some(#name);
                        self
                    }
                }
            }             
        });

    let build_validation = named_fields.iter().map(|f| {
        let name = f.ident.as_ref().unwrap();
        let field_name = name.to_string();
        // we need to check if the type is option, if so validation should not be necessary
        if f.is_optional() {
            quote!{
                let #name = self.#name.take();
            }
        } else if f.can_repeat() {
            quote!{
                let #name = self.#name.clone();
            }
        } else {
            quote!{
                let #name = self.#name.take().ok_or_else(|| concat!("Field `", #field_name , "` has not been set"))?;
            } 
        }
    });

    let build_self = named_fields.iter().map(|f| {
        let name = &f.ident;
        quote!{
            #name
        }
    });
    


    let name = i.ident;
    let builder_name = Ident::new(
        &format!("{}Builder", &name),
        Span::call_site()
    );

    
    //impl #builder_name { pub fn builder() {} }
        
    let c = quote!{
        impl #name {
            pub fn builder() -> #builder_name{
                #builder_name {
                    #( 
                        #defaults
                    ),*
                }
            }
        }
        
        pub struct #builder_name {
            #(
                #fields
            ),*
        }
        impl #builder_name {
            #(
                #setters
             )*
            pub fn build(&mut self) -> ::std::result::Result<#name, ::std::boxed::Box<dyn ::std::error::Error>> {
                #(#build_validation)*

                Ok(Command {
                    #(#build_self),*
                })

            }
             
        }
    };
    proc_macro::TokenStream::from(c)

}
