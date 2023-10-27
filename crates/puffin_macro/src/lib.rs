use proc_macro::TokenStream;
use heck::ToPascalCase;
use quote::{format_ident, quote};
use syn::{
    parse_macro_input,
    spanned::Spanned,
    ItemEnum, ItemStruct,
};

/// Generates an AST Node tree from an enum.
///
/// ## Using valid_for
/// The [`valid_for`] macro states which SyntaxKind's the nodes are valid for and can be cast from. Each variant in the enum must have one.
/// 
/// ## Variant transformation
/// It transforms each of the variants into a struct that implements AstNode and acts as a wrapper around SyntaxNode.
/// The variant's fields become the names of functions that search the internal SyntaxNode for a node of the provided field type
///
/// ## Enum transformation
/// The enum itself gets transformed into a struct that acts as a wrapper around a SyntaxNode. It also produces an enum called <original_enum_name>Kind
/// which allows for the struct to be interpreted as any one of the variants
///
/// ## Why a macro
/// Because its a lot to write out by hand and I wanted to learn about procedural macros. I also think it better shows the actual structure of the AST due
/// to its nesting
///
/// ## Example
/// This is an example of a simplified Stmt AST Node tree
/// ```rust
/// #[ast_enum]
/// enum Stmt {
///     #[valid_for(SyntaxKind::WHILE_STMT)]
///     WhileStmt{
///         body: BlockStmt
///     },
///     #[valid_for(SyntaxKind::BLOCK_STMT)]
///     BlockStmt {
///         stmt: Option<Stmt>
///     }
/// }
/// ```
/// This creates the BlockStmt and WhileNode wrapper structs which you can read more about in the [`ast_node`] macro. It also generates a struct for the enum
/// that looks like this
///
/// ```rust
/// /// This is the struct generated from the enum. Think of it representing an overarching syntax node category, such as statements in this case
/// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// pub struct Stmt {
///     syntax: crate::SyntaxNode
/// }
///
/// impl AstNode for Stmt {
///     fn can_cast(ty: crate::SyntaxKind) -> bool {
///         matches!(ty, SyntaxKind::WHILE_STMT | SyntaxKind::BLOCK_STMT)
///     }
///
///     fn cast(syntax: crate::SyntaxNode) -> Option<Self> {
///         if Self::can_cast(syntax.kind()) {
///             Some(Self { syntax })
///         } else {
///             None
///         }
///     }
///
///     fn syntax(&self) -> &crate::SyntaxNode {
///         &self.syntax
///     }
/// }
///
/// /// This represents the different ways that nodes in this category can be cast
/// #[derive(Debug, Clone, PartialEq, Eq)]
/// pub enum StmtKind  {
///     WhileStmt(WhileStmt),
///     BlockStmt(BlockStmt),
/// }
///
/// impl From<WhileStmt> for Stmt {
///     fn from(n: WhileStmt) -> Self {
///         Self { syntax: n.syntax }
///     }
/// }
///
/// impl From<BlockStmt> for Stmt {
///     fn from(n: BlockStmt) -> Self {
///         Self { syntax: n.syntax }
///     }
/// }
///
/// /// Anything that takes a Stmt as its node can have it become one of its kinds
/// impl Stmt {
///     pub fn kind(&self) -> StmtKind {
///         match self.syntax.kind() {
///             SyntaxKind::BLOCK_STMT => StmtKind::BlockStmt(BlockStmt::cast(self.syntax.clone()).unwrap()),
///             SyntaxKind::WHILE_STMT => StmtKind::WhileStmt(WhileStmt::cast(self.syntax.clone()).unwrap()),
///             _ => unreachable!(),
///         }
///     }
/// }
/// ```
#[proc_macro_attribute]
pub fn ast_enum(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemEnum);
    // Stores the produced items
    let mut produced_items = vec![];

    // Stores the name of each variant
    let mut enum_variant = vec![];
    // Stores the SyntaxKinds that each variant is valid for
    let mut enum_variant_valid_for = vec![];

    for variant in input.variants {
        // The information extracted from the variant
        let variant_name = variant.ident;
        enum_variant.push(variant_name.clone());
        let mut valid_syntax_kinds = vec![];

        let (search_functions, extra) = match extract_field_data(variant.fields) {
            Ok(FieldsGenerated::FieldData { impls, extra  } ) => {
                (Some(impls), Some(extra))
            },
            Ok(FieldsGenerated::NoFields) => {
                (None, None)
            }
            Err(err) => return err,
        };

        // Default implementation of syntax node or token
        for attr in variant.attrs {
            valid_syntax_kinds.append(&mut extract_valid_for(&attr));
            enum_variant_valid_for.push(extract_valid_for(&attr));
        }
        if search_functions.is_none() {
            produced_items.push(quote!(
                #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                pub struct #variant_name {
                    syntax: crate::SyntaxToken
                }

                impl AstToken for #variant_name {
                    fn can_cast(ty: crate::SyntaxKind) -> bool {
                        matches!(ty, #(#valid_syntax_kinds)|*)
                    }

                    fn cast(syntax: crate::SyntaxToken) -> Option<Self> {
                        if Self::can_cast(syntax.kind()) {
                            Some(Self { syntax })
                        } else {
                            None
                        }
                    }

                    fn syntax(&self) -> &crate::SyntaxToken {
                        &self.syntax
                    }
                }
            ));
        } else {
            // The generated code
            produced_items.push(quote!(
                #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                pub struct #variant_name {
                    syntax: crate::SyntaxNode
                }

                impl AstNode for #variant_name {
                    fn can_cast(ty: crate::SyntaxKind) -> bool {
                        matches!(ty, #(#valid_syntax_kinds)|*)
                    }

                    fn cast(syntax: crate::SyntaxNode) -> Option<Self> {
                        if Self::can_cast(syntax.kind()) {
                            Some(Self { syntax })
                        } else {
                            None
                        }
                    }

                    fn syntax(&self) -> &crate::SyntaxNode {
                        &self.syntax
                    }
                }

                impl #variant_name {
                    #search_functions
                }

                #extra
            ));
        }
    }
    let input_name = input.ident;
    let enum_kind_name = format_ident!("{}Kind", input_name);

    // Generate the match arms that link the <enum_name>Kind to the generated structs
    let mut match_arms = vec![];
    let matches: Vec<&syn::Path> = enum_variant_valid_for.iter().flatten().collect();
    for (count, each_enum_variant_valid_for )in enum_variant_valid_for.iter().enumerate() {
        let each_enum_variant = &enum_variant[count];
        match_arms.push(quote!(#(#each_enum_variant_valid_for)|* => {
            #enum_kind_name::#each_enum_variant(#each_enum_variant::cast(self.syntax.clone()).unwrap())
        }));
    }

    // The overarching AST node
    let enum_node = quote!(
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub struct #input_name {
            syntax: crate::SyntaxNode
        }

        impl AstNode for #input_name {
            fn can_cast(ty: crate::SyntaxKind) -> bool {
                matches!(ty, #(#matches)|*)
            }

            fn cast(syntax: crate::SyntaxNode) -> Option<Self> {
                if Self::can_cast(syntax.kind()) {
                    Some(Self { syntax })
                } else {
                    None
                }
            }

            fn syntax(&self) -> &crate::SyntaxNode {
                &self.syntax
            }
        }

        #[derive(Debug, Clone, PartialEq, Eq)]
        pub enum #enum_kind_name {
            #(#enum_variant(#enum_variant)),*
        }

        #(impl From<#enum_variant> for #input_name {
            fn from(n: #enum_variant) -> Self {
                Self { syntax: n.syntax }
            }
        })*

        impl #input_name {
            pub fn kind(&self) -> #enum_kind_name {
                match self.syntax.kind() {
                    #(#match_arms),*
                    _ => unreachable!(),
                }
            }
        }
    );
    // Combine them together
    quote!(
        #enum_node
        #(#produced_items)*
    ).into()
}

/// Convert a standalone struct into an AST Node that uses SyntaxNode as its internal representation.
/// Each field of the inputed struct will be converted into a function named after the field that casts the internal representation to the type of that field.
/// This means that each field's type must implement [`AstNode`]. There is also some additional special syntax for special cases.
///
/// ## Usage
/// ```rust
/// #[valid_for(SyntaxKind::WHILE_STMT)]
/// struct WhileStmt {
///     block: BlockStmt
/// }
/// ```
/// becomes
/// ```rust
/// #[derive(Debug, Clone, PartialEq, Eq, Hash)]
/// pub struct WhileStmt {
///     syntax: crate::SyntaxNode
/// }
/// 
/// impl AstNode for WhileStmt {
///     fn can_cast(ty: crate::SyntaxKind) -> bool {
///         matches!(ty, SyntaxKind::WHILE_STMT)
///     }
/// 
///     fn cast(syntax: crate::SyntaxNode) -> Option<Self> {
///         if Self::can_cast(syntax.kind()) {
///             Some(Self { syntax })
///         } else {
///             None
///         }
///     }
/// 
///     fn syntax(&self) -> &crate::SyntaxNode {
///         &self.syntax
///     }
/// }
/// 
/// impl WhileStmt {
///     pub fn block(&self) -> impl Iterator<Item = BlockStmt> {
///         crate::ast::children(self)
///     }
/// }
/// ```
/// ## Additional
/// If there are two fields with the same type, then instead of returning an iterator each of function returns the nth node of that type where n
/// is its position relative to the other fields have the same type in the struct.
///
/// ## Token
/// If there are no fields in the struct then it's assumed that its an [`AstToken`] and implements said trait
///
/// ## Significant tokens
/// If the node has significant tokens that affect how the node is interpreted, for example, the operand in a binary expression, then you can express it through
/// this syntax
/// ```rust
/// #[ast_node]
/// #[valid_for(SynaxKind::BIN_EXPR)]
/// struct BinExpr {
///     lhs: Expr,
///     op: (Add<SyntaxKind::PLUS>, Multiply<SyntaxKind::STAR>)
///     rhs: Expr,
/// }
/// ```
/// This produces an extra enum called OpKind and three extra functions for determining which OpKind the BinExpr is.
#[proc_macro_attribute]
pub fn ast_node(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let input = parse_macro_input!(item as ItemStruct);

    let name = input.ident;
    let mut valid_syntax_kinds = vec![];
    let (search_functions, extra) = match extract_field_data(input.fields) {
        Ok(FieldsGenerated::FieldData { impls, extra  } ) => {
            (Some(impls), Some(extra))
        },
        Ok(FieldsGenerated::NoFields) => {
            (None, None)
        }
        Err(err) => return err,
    };

    // Default implementation of syntax node or token
    for attr in input.attrs {
        valid_syntax_kinds.append(&mut extract_valid_for(&attr));
    }
    if search_functions.is_none() {
        quote!(
            #[derive(Debug, Clone, PartialEq, Hash)]
            pub struct #name {
                syntax: crate::SyntaxToken
            }

            impl AstToken for #name {
                fn can_cast(ty: crate::SyntaxKind) -> bool {
                    matches!(ty, #(#valid_syntax_kinds)|*)
                }

                fn cast(syntax: crate::SyntaxToken) -> Option<Self> {
                    if Self::can_cast(syntax.kind()) {
                        Some(Self { syntax })
                    } else {
                        None
                    }
                }

                fn syntax(&self) -> &crate::SyntaxToken {
                    &self.syntax
                }
            }
        ).into()
    } else {
        // The generated code
        quote!(
            #[derive(Debug, Clone, PartialEq, Eq, Hash)]
            pub struct #name {
                syntax: crate::SyntaxNode
            }

            impl AstNode for #name {
                fn can_cast(ty: crate::SyntaxKind) -> bool {
                    matches!(ty, #(#valid_syntax_kinds)|*)
                }

                fn cast(syntax: crate::SyntaxNode) -> Option<Self> {
                    if Self::can_cast(syntax.kind()) {
                        Some(Self { syntax })
                    } else {
                        None
                    }
                }

                fn syntax(&self) -> &crate::SyntaxNode {
                    &self.syntax
                }
            }

            impl #name{
                #search_functions
            }

            #extra
        ).into()
    }
}

/// Extracts the information from the valid_for macro
fn extract_valid_for(attr: &syn::Attribute) -> Vec<syn::Path> {
    let mut valid_syntax_kinds = vec![];
    if let Some(syn::PathSegment{ ident, ..}) = attr.path().segments.last() {
        if ident.to_string() == "valid_for" {
            // Extract the syntax kinds its valid for
            let _ = attr.parse_nested_meta(|meta| {
                valid_syntax_kinds.push(meta.path);
                Ok(())
            });
        }
    }
    valid_syntax_kinds
}

enum FieldsGenerated {
    FieldData {
        impls: proc_macro2::TokenStream,
        extra: proc_macro2::TokenStream,
    },
    NoFields
}

/// Extracts the field data and constructs the relevent extraction functions
fn extract_field_data(fields: syn::Fields) -> Result<FieldsGenerated, TokenStream> {
    let mut impls= vec![];
    let mut extra = vec![];
    let mut types: Vec<(syn::Type, syn::Ident)> = vec![];
    if fields.len() == 0 {
        return Ok(FieldsGenerated::NoFields);
    }
    for field in fields {
        if let Some(field_ident) = &field.ident {
            match &field.ty {
                syn::Type::Path(syn::TypePath{ path, ..}) => {
                    let field_ty = &field.ty;
                    if let Some(segment) = path.segments.last() {
                        // Check if its an option
                        if segment.ident == "Option" {
                            impls.push(quote!(
                                pub fn #field_ident(&self) -> #field_ty {
                                    crate::ast::possible_child(self)
                                }
                            ));
                        } else {
                            let count_matching_types = types.iter().filter(|t| t.0 == *field_ty).count();
                            // If there is a matching type we transform the current implementations into ones that grab the nodes in the order they appear
                            if count_matching_types > 0 {
                                let mut count: usize = 0;
                                for (idx, (_, ident)) in types
                                    .iter()
                                    .enumerate()
                                    .filter(|ty| ty.1.0 == *field_ty)
                                    .skip(1 - count_matching_types)
                                {
                                    // Update the impl to produce the actual node that it refers to
                                    impls[idx] = quote!(
                                        pub fn #ident(&self) -> Option<#field_ty> {
                                            crate::ast::children(self).nth(#count)
                                        }
                                    );
                                    count += 1;
                                }
                                types.push((field_ty.clone(), field_ident.clone()));
                                impls.push(quote!(
                                    pub fn #field_ident(&self) -> Option<#field_ty> {
                                        crate::ast::children(self).nth(#count)
                                    }
                                ))
                            } else {
                                types.push((field_ty.clone(), field_ident.clone()));
                                impls.push(quote!(
                                    pub fn #field_ident(&self) -> impl Iterator<Item = #field_ty> {
                                        crate::ast::children(self)
                                    }
                                ));
                            }
                        }
                    } else {
                        return Err(syn::Error::new(field.span(), "expected path to have a last segment").into_compile_error().into());
                    }
                },
                field_ty@syn::Type::Tuple(syn::TypeTuple{ elems, ..}) => {
                    types.push((field_ty.clone(), field_ident.clone()));
                    let extracted_tuple_data = extract_field_tuple_enum(elems, &field)?;
                    extra.push(extracted_tuple_data.generated_enum);
                    impls.push(extracted_tuple_data.extra_func);
                },
                field_ty@syn::Type::Paren(syn::TypeParen{elem, ..}) => {
                    // Interpret the type as a AstToken rather than AstNode
                    let count_matching_types = types.iter().filter(|t| t.0 == *field_ty).count();
                    // If there is a matching type we transform the current implementations into ones that grab the nodes in the order they appear
                    if count_matching_types > 0 {
                        let mut count: usize = 0;
                        for (idx, (_, ident)) in types
                            .iter()
                            .enumerate()
                            .filter(|ty| ty.1.0 == *field_ty)
                            .skip(1 - count_matching_types)
                        {
                            // Update the impl to produce the actual node that it refers to
                            impls[idx] = quote!(
                                pub fn #ident(&self) -> Option<#elem> {
                                    self.syntax.children_with_tokens()
                                        .filter_map(|it| it.into_token())
                                        .filter_map(|t| {
                                            #elem::cast(t)
                                        })
                                        .nth(#count)
                                }
                            );
                            count += 1;
                        }
                        types.push((field_ty.clone(), field_ident.clone()));
                        impls.push(quote!(
                            pub fn #field_ident(&self) -> Option<#elem> {
                                self.syntax.children_with_tokens()
                                    .filter_map(|it| it.into_token())
                                    .filter_map(|t| {
                                        #elem::cast(t)
                                    })
                                    .nth(#count)
                            }
                        ))
                    } else {
                        types.push((field_ty.clone(), field_ident.clone()));
                        impls.push(quote!(
                            pub fn #field_ident(&self) -> Option<#elem> {
                                self.syntax.children_with_tokens()
                                    .filter_map(|it| it.into_token())
                                    .find_map(|t| {
                                        #elem::cast(t)
                                    })
                            }
                        ));
                    }
                }
                _ => {
                    return Err(syn::Error::new(field.span(), "field type not handled").into_compile_error().into());
                },
            }
        } else {
            return Err(syn::Error::new(field.span(), "expected named field").into_compile_error().into());
        }
    }
    Ok(FieldsGenerated::FieldData {
        impls: quote!(#(#impls)*).into(),
        extra: quote!(#(#extra)*).into()
    })
}

struct MultipleToken {
    pub generated_enum: proc_macro2::TokenStream,
    pub extra_func: proc_macro2::TokenStream,
}

fn extract_field_tuple_enum(elems: &syn::punctuated::Punctuated<syn::Type, syn::token::Comma>, field: &syn::Field) -> Result<MultipleToken, TokenStream> {
    let mut token_kind_enum_variant_name = vec![];
    let mut token_kind_enum_variant_kind = vec![];
    // Extract the types contained within the tuple
    for ty in elems {
        if let syn::Type::Path(syn::TypePath{ path, ..}) = ty {
            if let Some(segment) = path.segments.last() {
                // The type name is the variant name and its generic argument is the SyntaxKind its valid for
                token_kind_enum_variant_name.push(&segment.ident);
                token_kind_enum_variant_kind.push(if let syn::PathArguments::AngleBracketed(
                    syn::AngleBracketedGenericArguments{ args, ..}) = &segment.arguments
                {
                    if let Some(syn::GenericArgument::Type(ty)) = args.first() {
                        ty
                    } else {
                        return Err(syn::Error::new(field.span(), "invalid path").into_compile_error().into());
                    }
                } else {
                    return Err(syn::Error::new(field.span(), "invalid path").into_compile_error().into());
                });
            } else {
                return Err(syn::Error::new(field.span(), "expected path to have a last segment").into_compile_error().into());
            }
        } else {
            return Err(syn::Error::new(field.span(), "field type not handled").into_compile_error().into());
        }
    }
    // Capitalise the first letter
    let field_ident = field.ident.clone().unwrap().to_string();
    let func_name = format_ident!("{}_details", field_ident);
    let func_name_2 = format_ident!("{}_kind", field_ident);
    let func_name_3 = format_ident!("{}_token", field_ident);
    let enum_ident = format_ident!("{}Kind", field_ident.to_pascal_case());
    let generated_enum = quote!(
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum #enum_ident {
            #(#token_kind_enum_variant_name),*
        }
    );
    let extra_func = quote!(
        pub fn #func_name(&self) -> Option<(crate::SyntaxToken, #enum_ident)> {
            self.syntax()
                .children_with_tokens()
                .filter_map(|it| it.into_token())
                .find_map(|c| {
                    let #func_name_2 = match c.kind() {
                        #(#token_kind_enum_variant_kind => #enum_ident::#token_kind_enum_variant_name),*,
                        _ => return None
                    };
                    Some((c, #func_name_2))
                })
        }
        pub fn #func_name_2(&self) -> Option<#enum_ident> {
            self.#func_name().map(|t| t.1)
        }
        pub fn #func_name_3(&self) -> Option<crate::SyntaxToken> {
            self.#func_name().map(|t| t.0)
        }
    );

    Ok(MultipleToken {
        generated_enum,
        extra_func,
    })
}

