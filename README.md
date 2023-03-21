# Puffin
A dynamic-ish scripting language designed to allow you to write code quickly while not shooting yourself in the foot.

## Why
Great question. I do have a few reasons for wanting to create my own language but the primary one
one is that I just really want to write my own programming language.
\
The other reason is because while I always liked the idea of dynamic programming languages in concept, allowing
you to quickly write code without worrying about types. But I find that while it makes the code easier to write
it also makes it more prone to errors and it becomes unclear how function that aren't properly documented are supposed to be used.

## Principles
The general rule is that if not specifying a type would lead you to have to check what that type is later, you must specify a type.

### Variables don't need to be typed.
A basic variable you use doesn't have to by typed and can act similar to variables from other dynamic languages.
```
var bob = 5
```
### You should always be able to specify a type.
If you want a variable to be statically typed than you can enforce it
```
var bob: int = 5

// Bob is an int and can't be set as a string
bob = "bob"
```

### Function parameters and return types must be typed
Whenever you call a function you pass in data. That data will always be the same type or you will check the type later in the
function and perform different functionality. If a function is left untyped and the wrong data is passed in it will fail, hence functions should always be typed to ensure that the correct data is passed in.
```
fun add(a: int, b: int) -> int {
    return a + b
}

var a = 5
var b = "Jane"

// Compile time error
add(a, b)

b = 4

// Puffin only checks that whatever b was last set to is a number
add(a, b)
```

### Struct fields must be typed
When using a struct, if we had untyped fields and wanted to access a field, we would have to check that its the type we expect,
hence structs must be typed
```
struct Point {
    x: int,
    y: int,
}
```

## Acknowledgements

### [Crafting interpreters](https://craftinginterpreters.com/)
If you ever want to write your own programming language. This is the place to start

