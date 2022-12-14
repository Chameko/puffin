# Puffin
An embeddible dynamic scripting language designed to perform the logic inside AVN.

## About
Puffin is a supplimentary language to Emu (the other language used in AVN) and is designed to
handle the logic, behaviour and control flow of the novel while Emu specifies the actual
writing.\
\
The reasoning for this is that you can focus on the writing of the story and what the characters
say in Emu and then later use Puffin to connect and create routes between the dialogue paths.

## Why
Great question. I do have a few reasons for wanting to create my own language but the primary one
one is that I just really want to write my own programming language. Other than that I did have
a look at some other potential options but found a few pet pieves that, while I'm certain I could
work around, ultimatly didn't defer my desire to hand craft my own language.

### [Rhai](https://rhai.rs/)
Probably one of the most mature rust scripting languages, however it's intended as a dynamic layer
over rust code and hence doesn't support the functionality I desired.

### [Wren](https://wren.io/)
On paper wren was the language for my use case. It was small and lightweight with a neat syntax
and had a class and inheritance system. The only thing stopping me was this project is written in
Rust and I wanted to do some intense itegration without the headache. In hindsight is this is
probably something I could have easily worked around... anyway

### [Rune](https://rune-rs.github.io/)
Rune was probably the best candidate for what I wanted and was dam near perfect. It had it all,
good Rust integration, structs, lots of beutiful functionality and even a VScode extention
with a language server. You know its a legit language when it has a VScode extention and a
language server. However I had but two tiny pieves. One, move semantics. I would like the language
to be as simple as possible and while probably the most insignificant problem ever devised move
symantics just make writing some things more finicky than I'd like. The ability to also restrict
function parameter types and return types is also something I'd like as it would allow me to
define a nice API with explicit type information.\
If you like Puffin but would like something more mature, Rune is probably it.

## Design
One of my main issues with many Rust based scripting languages is that they bring Rust like
semantics into a space which I personally don't think should have it. The point of a scripting
language is a interface to your program/library in a more accessible way. Rust's borrowing
mechanics are not accessibly and can be rather confusing. The last thing you want is someone
trying to scripts something only for the compiler to go "nonononono. we do not do that here."
with very little understanding as to **WHY** they can't. So that was the first rule:\
**PUFFIN. IS. NOT. RUST.**\
Great. Now it doesn't mean I forbid any rust from seeping in. For example, stealing Rust's
approach to objects and lack of classes. This is personally because I think they are simpler
and will provide 99% of the functionality anyone will need with a language like this.\
Puffin uses a byte code interpreter and is reference counted.

## Syntax
I wanted Puffin to be friendly to beginners so the syntax is a mashup of Rust and Python with a
few keywords renamed.

## Acknowledgements

### [Crafting interpreters](https://craftinginterpreters.com/)
Huge thanks to Robert Nystrom for writing Crafting Interpreters. If you ever want to write your
own programming language. This is the place to start

### [Rune](https://rune-rs.github.io/)
Big thanks to udoprog and the contributers behind Rune. A lot of insiration was taken its source
code, which is extremely well documented (its actually really impressive). If you want to write
your own language in Rust I highly encourage you to read its source.

### [Rust Hosted Langs](https://rust-hosted-langs.github.io/book/)
The original heap of Puffin was an implementation of sticky immix from this guide and while
it didn't make it into the language itself it would be wrong not to mention it.

