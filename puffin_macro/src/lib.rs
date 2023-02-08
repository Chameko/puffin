use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::{format_ident, quote, ToTokens};
use syn::{
    parse::{Parse, Parser},
    parse2, parse_macro_input,
    spanned::Spanned,
    Fields, ItemEnum, ItemStruct, Token, Variant,
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

struct ASTEnumException {
    pub ignored: Vec<Ident>,
    pub boxed: Vec<Ident>,
}

impl ASTEnumException {
    fn parse_list(input: &syn::parse::ParseStream) -> syn::Result<Vec<Ident>> {
        let mut idents = vec![];
        while input.peek2(Token![,]) {
            idents.push(input.parse::<Ident>()?);
            input.parse::<Token![,]>()?;
        }
        idents.push(input.parse::<Ident>()?);
        Ok(idents)
    }
}

impl Parse for ASTEnumException {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let mut ast_enum_exception = Self {
            ignored: vec![],
            boxed: vec![],
        };
        let mut cont = true;
        while cont {
            if let Ok(phrase) = input.parse::<Ident>() {
                if phrase == "ignore" {
                    input.parse::<Token![=]>()?;
                    ast_enum_exception.ignored = Self::parse_list(&input)?;
                } else if phrase == "boxed" {
                    input.parse::<Token![=]>()?;
                    ast_enum_exception.boxed = Self::parse_list(&input)?;
                } else {
                    return Err(syn::Error::new(phrase.span(), "Expected valid phrase"));
                }
            }
            if input.parse::<Token![|]>().is_err() {
                cont = false;
            }
        }
        Ok(ast_enum_exception)
    }
}

/// Takes a enum with struct-based variants and creates a struct from said variants, implementing some common functionality into them.
/// ## Arguments
/// This macro can take a list of ignored variants using ignore = Var1, Var2 and can also box the struct
/// in the variants if the struct is recursive using boxed = Var1, Var2. These arguments lists are seperated
/// by the pipe symbol
/// ### Example
/// \#\[puffin_macro::ast_enum(ignore = ExprStmt | boxed = If)\]
#[proc_macro_attribute]
pub fn ast_enum(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut input = parse_macro_input!(item as ItemEnum);
    let special = parse_macro_input!(attr as ASTEnumException);

    let enum_name = input.ident.clone();

    // Prepare array of generated structs
    let mut gen_struct: Vec<TokenStream> = vec![];
    for variant in &mut input.variants {
        // Ignore any variants specified
        let mut cont = false;
        for ig in &special.ignored {
            if variant.ident == *ig {
                cont = true;
                break;
            }
        }
        if cont {
            continue;
        }

        // Structs name will be a combination of the variants name and the enum name
        let variant_name = variant.ident.clone();
        let struct_name = format_ident!("{}{}", variant_name, enum_name);
        // We keep the idents and types separate as they are used for function signatures and the fields
        // type is to ensure all relevant field data gets transferred to the new struct
        let mut field_idents = vec![];
        let mut field_types = vec![];
        let mut struct_fields = vec![];
        // Gather the field idents and types
        if let Fields::Named(ref mut fields) = variant.fields {
            for field in &fields.named {
                struct_fields.push(field);
                if let Some(ident) = &field.ident {
                    field_idents.push(ident.clone());
                }
                field_types.push(field.ty.clone())
            }
        }

        // Add necessary surrounding box if required
        let mut potential_box = quote!(value);
        let mut potential_variant = parse2::<Variant>(quote!( #variant_name(#struct_name) ));
        for id in &special.boxed {
            if variant.ident == *id {
                potential_box = quote!(std::boxed::Box::new(value));
                potential_variant =
                    parse2::<Variant>(quote!( #variant_name(std::boxed::Box<#struct_name>) ));
            }
        }

        // Create the new struct from the variant
        let enum_struct = quote!(
            #[derive(Debug, PartialEq, Clone)]
            pub struct #struct_name {
                pub range: std::ops::Range<usize>,
                #(#struct_fields),*
            }

            impl #struct_name {
                pub fn new(range: std::ops::Range<usize>, #(#field_idents: #field_types),*) -> Self {
                    Self {
                        range,
                        #(#field_idents),*
                    }
                }

                pub fn ast_node(range: std::ops::Range<usize>, #(#field_idents: #field_types),*) -> #enum_name {
                    let value = Self {
                        range,
                        #(#field_idents),*
                    };
                    #enum_name::#variant_name(#potential_box)
                }

                pub fn test_node(#(#field_idents: #field_types),*) -> #enum_name {
                    let value = Self {
                        range: 0..0,
                        #(#field_idents),*
                    };
                    #enum_name::#variant_name(#potential_box)
                }
            }

            impl crate::common::ast::TestCmp for #struct_name {
                fn test_ast_cmp(&self, b: &Self) -> bool {
                    (true #(&& self.#field_idents.test_ast_cmp(&b.#field_idents))*)
                }
            }
        );

        gen_struct.push(enum_struct.into());

        // Rewrite the variant to use the struct
        if let Ok(var) = potential_variant {
            *variant = var;
        } else {
            // Report error if we fail to parse the variant
            gen_struct.push(
                syn::Error::new(variant.to_token_stream().span(), "Failed to create variant")
                    .to_compile_error()
                    .into(),
            );
        }
    }

    let variants = input
        .variants
        .iter()
        .map(|v| v.ident.clone())
        .collect::<Vec<Ident>>();

    let enum_impl = quote!(
        impl crate::common::ast::TestCmp for #enum_name {
            fn test_ast_cmp(&self, b: &Self) -> bool {
                match self {
                    #(#enum_name::#variants( node1 ) => {
                        if let #enum_name::#variants( node2 ) = b {
                            node1.test_ast_cmp(node2)
                        } else {
                            false
                        }
                    }),*
                }
            }
        }
    );

    // Combine the token streams
    let mut output: TokenStream = input.to_token_stream().into();
    for stream in gen_struct {
        output.extend(stream);
    }
    output.extend::<TokenStream>(enum_impl.into());

    output
}

/// Adds necessary range field to the token as well as some helper functions for constructing the nodes themselves
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
                .parse2(quote! { pub range: std::ops::Range<usize> })
                .unwrap(),
        );
    }

    let rtrn = quote! {
        #input
        impl #name {
            pub fn new(range: std::ops::Range<usize>, #(#idents: #types),*) -> Self {
                Self {
                    range,
                    #(#idents),*
                }
            }
        }
    };

    let extra = match path {
        Surround::Box(var) => {
            let variant = format_ident!("{}{}", name, var);
            let value = quote!(std::boxed::Box::new(value));
            quote!(
                impl #name {
                    pub fn ast_node(range: std::ops::Range<usize>, #(#idents: #types),*) -> #var {
                        let value = Self {
                            range,
                            #(#idents),*
                        };
                        #var::#variant(#value)
                    }

                    pub fn test_node(#(#idents: #types),*) -> #var {
                        let value = Self {
                            range: 0..0,
                            #(#idents),*
                        };
                        #var::#variant(#value)
                    }
                }
            )
        }
        Surround::Unboxed(var) => {
            let variant = format_ident!("{}{}", name, var);
            let value = quote!(value);
            quote!(
                impl #name {
                    pub fn ast_node(range: std::ops::Range<usize>, #(#idents: #types),*) -> #var {
                        let value = Self {
                            range,
                            #(#idents),*
                        };
                        #var::#variant(#value)
                    }

                    pub fn test_node(#(#idents: #types),*) -> #var {
                        let value = Self {
                            range: 0..0,
                            #(#idents),*
                        };
                        #var::#variant(#value)
                    }
                }
            )
        }
        _ => quote!(),
    };

    quote!(
        #rtrn
        #extra
    )
    .into()
}
