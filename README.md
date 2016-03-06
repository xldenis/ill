# Test Language Specification

A simple, imperative, strongly-typed language exploring different compiler techniques.

## Features:

- Term level typing
- Global inference?
- Mutable state
- No global state
- no statements
- first class functions
- anonymous functions?
- LLVM backend
- Fast performance
- multithreading
- structural typing?
- gradual typing?
- pattern matching
- partial application

## Confirmed Features

- local inferrence
- mutable state
- no global state
- no statements
- first class functions
  - lambdas
- llvm
- decent performance
- simple parallelism / concurrency
- pattern matching
- partial application / currying

## Possible Features
- global inference
- gradual typing
- structural typing
- fast performance

## Example Syntax:


```
fn name(args: type) -> type
  x = 1
  x, y = 2, {2 => 3} # multi-assign
end # returns ??

fn name(args) # inferred types
  body = 2
end # returns 2

fn (args) # lambda

end
```

## Literals

- Hash: `{}`
- Array: `[]`
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

The return type can be elided.


