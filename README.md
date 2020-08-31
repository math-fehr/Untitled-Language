# ULM Language

The Uniform Linear Monadic Language aim to merge three core features that we
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
only start actually working on it about 10 days ago, so we've only had half the
time of the JAM to do this. I'll try to mark clearly what is already done and
what is planned in this description/documentation.

We already have a REPL working at TODO.

Let's discuss the core features:

## Uniform

The language being Uniform means that everything is a value. integers are
values, functions are values, types are value, modules are values. A makepair
function can a build a pair of anything with anything. For now modules (similar
to Ocaml) are only a planned features, but we already have first order-types.

With first order types comes full-metaprogramming facilities. At compile-time
types can be passed around and worked on like any other first-order value, like
in Zig and Julia. Sentences like "The C++ type-system is Turing-complete" are
unessessary because the type-system is manipulated with the language which is
obviously Turing-complete. You can perfectly do:

```
match type with
  | Array n elem_type => ...
  | Option typ => ...
  | Int => ...
  | _ => ...
```

Obviously, the type `Type` can be extended and so any function matching on types
must have a wildcard (`_`) pattern.

At same time this implies the full power of dependent types at compile time with
type constructor possible depending on nearly every value type (including `Type`
obviously). Some restrictions applies. Therefore there a full-blown compile time
interpreted for all values including type that can be called.

## Linear

The goal of this low-level language is also to allow low-level manipulation of
memory in a safe and pure manner. Additionally, we also want to provide a safe
way to manipulate other resources like files, sockets, locks, etc. We thus
require a type system that can make a difference between resources and simple
information like an integer that can be passed around. Luckily for us such a
type system already exists and is called a linear type system which stems from
linear logic. Those that have a computer science background but don't know
linear logic can look [there](https://en.wikipedia.org/wiki/Linear_logic). Here
I'll explain how it works in our language in way that can be understood by
programmers that don't know this theory.

Each variable and expression is tagged "linear" if it contains at least one
resource inside and is tagged "non-linear" if it contains only pure information
and no resources. The linear type systems ensures that linear values cannot be
duplicated or deleted. They only exists in a single variable at any moment.

TODO


## Monads

Unfortunately. TODO explain the idea and then do cf. Haskell

# Getting Started

TODO Explain how to setup the REPL

# Current primitives and syntax

TODO Give example like fibonacci and how to use tuple, arrays, enums, ints, ...

# Smaller Planned features

## Module and Typeclasses

## Overloadable match

## Full type introspection

## Pattern-matching Forall

## Memory-management
