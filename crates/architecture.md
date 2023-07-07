# Architecture

Here you can find a guide to what each crate does and why.

## Tools

Theses are tools used to generate the various structs used for representing the syntax tree. The code here was primarily taken from
Mun's codegen.

## Puffin AST

Contains the various structs for both the AST and the CST. The concrete syntax tree does not dispose of any detail such as whitespace or newlines. This means that we
could effectivly reconstruct the entire source file from the CST. Under the hood Puffin uses the rowan crate for this. The abstract syntax tree or AST is only conserned
with the operations that occur in the source code and hence don't contain detail such as whitespace. The AST is constructed from the CST.  
The primary reason for having a CST is because it tools such as language servers require the full code detail when performing actions and analysing the code. Hence
to prevent writing the parser twice we use both and the AST is generated from the CST. I recommend reading rust-analyzers architecture for more detail and a (probably)
better implementation.

## Puffin VFS

The virtual file system for puffin.

## Puffin VM

The virtual machine for puffin. Its a stack based machine because they're easier to write and is a good compramise between tree-walk interpreter and binary.

## Puffin Parser

Puffin has two parsers, one for the CST and one for the AST. The CST parser is the "main" parser and is a  recursive descent pratt parser. It also allows errors
as the CST is capable of containing error tokens, primarily for the language server. The AST parser isn't really a parser and is just designed to convert a CST
to an AST. However if it runs into an error it always panics because every valid CST **MUST** be a valid AST. If the AST parser fails its a fatal error and something
is wrong and needs to be fixed.

## Puffin Codegen

This is responsible for transforming the AST into the bytecode for the VM.
