# Architecture

A general guide to puffin's architecture and how your code gets transformed from text to a working program.

## Tools

Theses are tools used to generate the various structs used for representing the syntax tree. The code here was primarily taken from
Mun's codegen.

## Crates

See the crate level documentation for what each crate is responsible for

## Data

How data flows

1. Src file is loaded
Note: I need a better source struct

2. Src data is tokenised

3. Src data gets constructed via rowan

4. Create CST that TIGHTLY wraps around rowan with various convenience functions

5. Create AST that drops trivia and contains additional information such as typing

6. Resolve into bytecode
