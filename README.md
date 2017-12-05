# Ill: I was sick when I started this project

A simple, strongly-typed language exploring different compiler techniques.

## Current commands and usage

Build the executable using `stack build`, it can then be run with `stack exec ill`. The following commands are available:

```
ill infer file/path # run the type checker on the specified module. Outputs the types and kinds of everything.
ill format file/path # run the pretty printer on a module. Outputs pretty printed code.
ill desugar (traits|cases) file/path # run the desuraging pipeline up to the specified pass. Outputs the transformed module.
ill core file/path # runs the full desugaring pipeline and outputs generated core module.
ill run file/path # interpret a module using a Call-By-Name interpreter.
ill build file/path # not yet implemented
```

## Purpose

When I first started working on this project, I was in it solely for myself. I was fascinated by compilers but only knew (and it still applies) a little of how compilers worked. My only real experience before was writing a compiler for a subset of Go as a final project for my compilers course at McGill. So I set out to build a _full_ compiler for a functional language. Originally, I was going to cram the language with gradual types, linear types, dataflow parallelism, laziness/strictness polymorphism, etc... Then reality hit, and I had to realize I would get nothing accomplish if I didn't scope things down, a lot. So now, `ill` is a Haskell98-like language, with no real fancy language features.

As the project went on, I realized that there is a gap in the resources to learn about functional compilers. There are a near infinitude of toy compilers and there quite a few production compilers, but very little in between. Toy compilers are great for showing off the core of an idea but they often leave out the little details that turn out to be most of the work. On the other hand, production compilers are so full of those little details that the core idea is obscured. I hope to turn `ill` into a project sitting squarely in the middle-ground between those extremes.

The idea is that `ill` should implement a _real_, pure, functional language, one in which a developer could _conceivably_ write software in a simple and modern manner. In a lot of ways I'm hoping this can be viewed as a spiritual continuation of Stephen Diehl's Kaleidoscope or Write You a Haskell projects. The code should be well documented (lol), and idiomatic. The techniques and algorithms used in compilation should be modern, they may not be _cutting-edge_ but they should still be relevant in 2017. When necessary, the code should opt for simplicity and separation of phases over performance. The major phases should be well separated from each other, so that the typechecker, desugaring, codegen, etc... can all be understood independently from each other.

## Some more details on the language

Here are a few of the fixed aspects of the language:

- Hindley-Milner + Type classes for the type system
- Desugaring to System-F Core language
- Compiles to LLVM
  - Some sort of garbage collection (ideally precise)
- Provides a REPL
- Module system

Notice that laziness is _not_ a fixed requirement. If possible, I'd like to provide both lazy and strict backends to the language so the approaches can easily be compared to each other.

## Contributions

I'll happily accept contributions in the form of pull requests. This is a personal project though so I won't guarantee code quality (yet) or documentation.

## State of the Compiler

So far, I've advanced quite a lot in development. Here are the finished phases

- Parsing
  - Uses megaparsec and provides annotated errors on parses
- Type checking
  - Simple bidirectional type checker, solves and simplfies constraints, annotates source tree
- Desugaring of traits
- Desugaring of pattern matching
- Desugaring to Core
  - A full desugaring of terms to the Core representation
- Core Linter
- Call-By-Need interpreter

Here are some of the major phases that remain to be done:

- Renaming
  - There is no renaming currently since programs can only consist of one module.
- Eta-expansion of core
  - Required to simplify code generation
- Code generation
  - This will involve more sub-phases but those haven't been decided on
- Module build system
  - Some system to find, and organize the module build plan.


## Example Syntax:


```
name :: Nnt -> Int
fn name(args)
  x = 1
  x, y = 2, 4 # multi-assign
  y
end # returns 4

fn name(args) # inferred types
  body = 2
end # returns 2

fn (args) # lambda

end
```

## Literals

- ~~Array: `[]`~~ _Not Yet Implemented_
- Integers: `1`
- Floats: `2`
- Literal Strings `'string \n'`
- Escaped strings `"string \n"`

## Function definition

There are two different manners to declare functions, either anonymous functions (lambda) or named functions. A function signature consists of the argument list and the return type.

### Argument list

Arguments are patterned matched and can take several forms.

- Variables `a`
- Destructors `Node t1 t2`
- Wildcards `_`

Additionally they can have type hints or annotations `fn (x :: int, Node a b :: Tree Int)`


