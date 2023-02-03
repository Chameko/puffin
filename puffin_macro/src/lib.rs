use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::{format_ident, quote};
use syn::{
    parse::{Parse, Parser},
    parse_macro_input, ItemStruct, Token,
};

enum Surround {
    Box(Ident),
    Unboxed(Ident),
    Ignore,
}

impl Parse for Surround {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        if input.peek(Token![<]) {
            input.parse::<Token![<]>()?;
            let rslt = input.parse::<Ident>()?;
            input.parse::<Token![>]>()?;
            Ok(Surround::Box(rslt))
        } else if input.peek(Token![_]) {
            input.parse::<Token![_]>()?;
            Ok(Surround::Ignore)
        } else {
            let rslt = input.parse::<Ident>()?;
            Ok(Surround::Unboxed(rslt))
        }
    }
}

/// Adds neccessary range field to the token as well as some helper funcitons for constructing the nodes themselves
/// Note: It assumes that each variant is the node with the enums name (for example Expr or Stmt) as a suffix
/// Could I have done this using traits and generics. Yes. But I would have to do it by hand and this is
/// way cooler :)
#[proc_macro_attribute]
pub fn ast(attr: TokenStream, item: TokenStream) -> TokenStream {
    let path = parse_macro_input!(attr as Surround);

    let mut input = parse_macro_input!(item as ItemStruct);
    let name = input.ident.clone();
    let mut idents = vec![];
    let mut types = vec![];
    if let syn::Fields::Named(ref mut fields) = input.fields {
        for field in &fields.named {
            if let Some(ident) = &field.ident {
                idents.push(ident.clone());
            }
            types.push(field.ty.clone())
        }

        fields.named.push(
            syn::Field::parse_named
                .parse2(quote! { range: std::ops::Range<usize> })
                .unwrap(),
        );
    }

    let rtrn = quote! {
        #input
        impl #name {
            pub fn new(#(#idents: #types),*, range: std::ops::Range<usize>) -> Self {
                Self {
                    #(#idents),*, range
                }
            }
        }
    };

    let extra = match path {
        Surround::Box(var) => {
            let variant = format_ident!("{}{}", name, var);
            quote!(
                impl #name {
                    pub fn ast_node(#(#idents: #types),*, range: std::ops::Range<usize>) -> #var {
                        let value = Self {
                            #(#idents),*, range
                        };
                        #var::#variant(std::boxed::Box::new(value))
                    }

                    pub fn test_node(#(#idents: #types),*) -> #var {
                        let value = Self {
                            #(#idents),*, range: 0..0
                        };
                        #var::#variant(std::boxed::Box::new(value))
                    }
                }
            )
        }
        Surround::Unboxed(var) => {
            let variant = format_ident!("{}{}", name, var);
            quote!(
                impl #name {
                    pub fn ast_node(#(#idents: #types),*, range: std::ops::Range<usize>) -> #var {
                        let value = Self {
                            #(#idents),*, range
                        };
                        #var::#variant(value)
                    }

                    pub fn test_node(#(#idents: #types),*) -> #var {
                        let value = Self {
                            #(#idents),*, range: 0..0
                        };
                        #var::#variant(value)
                    }
                }
            )
        }
        Surround::Ignore => quote!(),
    };

    quote!(
        #rtrn
        #extra
    )
    .into()
}
