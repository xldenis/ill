# Test Language Specification

A simple, strongly-typed language exploring different compiler techniques.

## Current commands and usage

Build the executable using `stack build`, it can then be run with `stack exec ill`. The following commands are available:

```
ill infer file/path # run the type checker on the specified module. Outputs the types and kinds of everything.
ill format file/path # run the pretty printer on a module. Outputs pretty printed code.
ill desugar (traits|cases) file/path # run the desuraging pipeline up to the specified pass. Outputs the transformed module and any core bindings that can be generated.
ill run file/path # interpret a module using a Call-By-Name interpreter.
ill build file/path # not yet implemented
```

## ideas:

- Fast performance
- structural typing?
- gradual typing?
- simple parallelism / concurrency
- mutable state
  - via linear types?

## planned features

- type inferrence / checking
- purity
- first class functions
  - lambdas
- llvm
- decent performance
- pattern matching
- partial application / currying

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

- ~~Hash: `{}`~~ _Not Yet Implemented_
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


