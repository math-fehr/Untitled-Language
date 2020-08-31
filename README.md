# ULM Language

The Uniform Linear Monadic Language aims to merge three core features that we
think would be useful for reliable and safe software. We aim to bridge the gap
between, on one hand, functional languages like Haskell and OCaml where
everything is nice and "simple" but opaque, and on the other hand, low-level
language like Rust, C/C++ and Zig where you know exactly what the machine is
doing but the presence of side-effect and memory-safety problems (which Rust
remove in most part) complicate a lot the thinking process. This language thus
intended to stay pure like Haskell with absolutely zero direct-side effect, but
it also intend to allow manipulation of raw memory in a direct but safe manner.
This language is inspired from Rust, C++, Haskell, OCaml, Zig and Julia. The
syntax itself is mostly similar the ML family of language, in particular Ocaml.

We are mostly targeting experienced programmer and low-level and fast program,
like kernels, compilers, ... Obviously this language is designed to be compiled,
but due to the timeframe constraints of the JAM, we currently only have an
interpreter. Having an interpreter is not a waste of time because it helps
development and is required anyway during the compilation process.

The ULM language is really unfinished. Because of various constraints, we could
only start seriously working on it about 10 days ago, so we've only had half the
time of the JAM to do this. We will try to mark clearly what is already done and
what is planned in this description/documentation.

We already have a REPL working, See the "Getting Started" section

Let's discuss the core features:

## Uniform

The language being Uniform means that everything is a value. integers are
values, functions are values, types are value, modules are values. A `makepair`
function can a build a pair of anything with anything. For now modules (similar
to Ocaml) are only a planned features, but we already have first order-types.

With first order types comes full-metaprogramming facilities. At compile-time
types can be passed around and worked on like any other first-order value, like
in Zig and Julia. Sentences like "The C++ type-system is Turing-complete" are
unnecessary because the type-system is manipulated with the language which is
obviously Turing-complete. You can perfectly do:

```
match type with
  | Array n elem_type => ...
  | Int => ...
  | _ => ...
```

Obviously, the type `Type` can be extended and so any function matching on types
must have a wildcard (`_`) pattern.

At same time this implies the full power of dependent types at compile time with
type constructor possible depending on nearly every value type (including `Type`
obviously). Some restrictions applies. Therefore there is need for a full-blown
compile time interpreter for all compile-time values including types.

## Linear

The goal of this low-level language is also to allow low-level manipulation of
memory in a safe and pure manner. Additionally, we also want to provide a safe
way to manipulate other resources like files, sockets, locks, etc. We thus
require a type system that can make a difference between resources and simple
information like an integer that can be passed around. Luckily for us such a
type system already exists and is called a linear type system which stems from
linear logic. Those that have a computer science background but don't know
linear logic can look [there](https://en.wikipedia.org/wiki/Linear_logic). Here
We'll explain how it works in our language in way that can be understood by
programmers that don't know this theory.

Each variable and expression is tagged "linear" if it contains at least one
resource inside and is tagged "non-linear" if it contains only pure information
and no resources. The linear type systems ensures that linear values cannot be
duplicated or deleted. They only exists in a single variable at any moment. This
is very similar to the Rust language which also use linear type theory.

In practice functions are explicitly typed depending on their interaction with
linearity. If we have `f : a -> b` that means that `f` only deals with
non-linear types as if it only deals with information. It is thus free to copy
the value of type `a` as much at it wants and to put multiple such values in
`b`. On the calling side, the caller must call `f` on an argument that is tagged
as non-linear but it can allow to tag the result as non-linear.

On the other hand, if we have `f : a -@ b`, that means that `f` deals with
linear type i.e. resources. This contract guarantees that `f` uses `a` only
once and that `a` appear once in `b`. In this case `f` can be called on any
variable, linear or not. If `f` is called on a non-linear variable it's output
can be used in non-linear ways at will.

On hindsight, this system is insufficient to handle all the use cases we thought
of, so we'll have to change it quite massively for it to be usable in the real
world. In particular, we'll move to include the linearity inside the types.

## Monads (Not yet implemented)

Linear types are not sufficient to implement actual side-effect on the external
world like printing on `stdout` or reading from `stdin`. In order to do that, we
need an `IO` monad like the one of Haskell and a more general monad theory. This
had not yet been implemented by lack of time. One thing that the combination of
a linear type system and monad allow is for example to be able to extract
`stdout` as a linear ressource type from `IO` and then pass it around as a
linear value, Using it in function like `printint : OutStream -@ Int ->
OutStream` that consume and use it to print the integer, then return the output
stream after the effect. This allow some limited side-effect power without the
syntax of monads, while staying a pure language

# Getting Started

We only have a REPL for now. However you can load files in the REPL
with `:l filename`.

## With stack

Do `stack init` then `stack run Untitled-Language` (the language was still named
"Untitled-language at the project creation time). You now have a REPL !

## With Nix

You can build and run the REPL using
[nix flakes](https://nixos.wiki/wiki/Flakes). To do so, you need to
ensure that your version of Nix support flakes, and run the following
:

```shell
$ nix build
$ ./result/bin/Untitled-Language
```

# Current primitives and syntax

## Type system remarks

The type system is fully accessible at compile time, all expression that can be
evaluated at compile time are evaluated then as they are pure and cannot have
side-effects. Since arbitrary operation can be done on types, Type inference is
not possible in general. We choose not to do it at all, which means types are
deduced in a purely forward manner: Later types appearing later in a function
definition cannot affect in any way the type of previous variable declaration.

## Basic syntax

### Expressions

Expressions are like usual language for arithmetic like `1 + 1` but are also
available for types like `Int & Int` which the type of a pair of int:
```
(2 , 3) : Int & Int
```
The `&` operator was chosen because we wanted `*` to remain associative in all cases.
`&` is not an associative operator `a & b & c` is not `(a & b) & c` neither `a & (b & c)`.
This is necessary to implement fundamentally not associative operation like constructing
a tuple types. Such operators are called many-arity operators.

Function are called in the functional style like `function argument` and are generally
curried.

### Primitive types

Constant integer are of type `Int` which is temporarily an unbounded integer.
There is also a type `Bool` with values `True` and `False`.

### Arrays

Fixed-size array are (for now) a primitive type. They are currently our only example
of dependent types but in the future any type could be dependent.

The construction syntax is:
```
{1 ; 2 ; 3} : Array Int 3
```

Here `Array` is a function of type `Type -> Int -> Type`.

Array can be indexed with `array[index]` like usual. This is only a getter as the
language is pure.

### Tuples

Tuples is group of values of arbitrary types. A tuple is constructed with the
comma operator `,` and the type of a tuple is constructed with `&` on types. For
example:

```
1 , False, {5 ; 6}, (True, 42) : Int & Bool & Array Int 2 & (Bool & Int)
```

Tuple can be indexed with `tuple[index]` with the condition that the `index` is
a compile time constant, otherwise the type of the expression could not be
deduced.

### Control-flow

The syntax of the `if` is the usual one of functional programming language :
```
if condition then expr1 else expr2
```

Unfortunately there is no generic match construction for now. The match
construct can only be used to deconstruct enums

### Function declaration

Since the type system is purely forward, a function must declare explicitly the type of all
it's arguments. The syntax is

```
decl func : Type1 -> Type2
def arg := body
```

However a function could use another function in it's type definition like

```
decl func : computetypeoffunc arg1 arg2
def arg := body
```

During the typing of `func`, `computetypeoffunc` is fully evaluated and the
result must be of type `Type`, this value of type is then used as the type of
`func`. In fact, in the previous declaration, `Type1 -> Type2` was a normal
expression that used the operator `->` on `Type1` and `Type2`. The operator `->`
and `-@` have types `Type -> Type -> Type`.

### Let binding

To introduce new variables, one can do

```
let var := expr in body
```

This will evaluate `expr`, bind it to `var` and then evaluate `body` using
`var`. There is also a typed version that looks like this:

```
let var : typeexpr := expr in body
```

In this one, `typeexpr` is evaluate at compile time, and it's value which must
be of type `Type` is checked against the type of `expr`.

### Lambda function

The syntax for lambda/anonymous function is:

```
fun arg => body
```

### Enumeration

The syntax for enumerations is:

```
enum optionInt : Type
def := { Some : (Int -@ optionInt) | None : optionInt }
```

They can be pattern-matched on with the syntax:

```
match Some 0 with
| Some a => a
| None => 12
end
```

We plan to add type arguments on enumerations, with the following syntax:
```
enum option : Type -> Type
def A := { Some : (Int -@ option A) | None : Option A}
```

with the following pattern-match syntax:
```
match Some Int 0 with
| Some a => a
| None => 23
end
```

Note that we also aim to infer when possible the type arguments, so we could write:
```
match Some 0 with 
| Some a => a
| None => 23
end
```

## Examples

### Fibonacci

```
decl fib_naive : Int -> Int
def n = if n == 0 then 1 else fib (n -1) + fib (n -2)
```

### First-order types

```
decl type : Int -> Type
def x := if x == 0
           then Int
           else Bool

decl fint : type 0
def := 42

decl fbool : type 1
def := True
```

TODO more examples

# Smaller planned features

 - Module and Typeclasses: We plan to implement a module system similar to Ocaml's
   but to support typeclasses.
 - Overloadable match: The match will overloadable in certain ways. For example the user will
   be able to define how to deconstruct a type with a precise constructor even if the constructior
   is not one of the type. This allow continuity in APIs based on deconstructions.
 - Full type introspection: We'll expand the standard library on types and the used will
   be able to deconstruct a type in match like any other value.
 - Currently it is not possible to express polymorphic function which make the language
   quite unusable for non toy example, we plan to add explicit polymorphism and compile
   it with monomorphisation.
 - Memory-management: We plan to add low-level memory management in a way protected by
   the linear type system.
