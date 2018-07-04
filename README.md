# Bracky-lang

*This repository is currently at very early stage and inactive.*

**Bracky-lang** is a programming language with *humorous* syntax in the middle of ML and LISP.

## Syntax

Code looks like the following:

```fsharp
// Bracky code (highlighted as F# though)

{ val fizz_buzz n =
  { if n % 15 == 0 -> -15
    if n % 3 == 0 ->
      {val fizz = -3};
      fizz
    if n % 5 == 0 ->
      {val buzz = -5};
      buzz
    else n
  }
}
```

Because `if` expressions occur directly inside of `{}`, the first `if` in an if/else-if chain is distinct from second `if`s. So all conditions are aligned at the same column.

## Semantics

- Types:
    - unit
    - bool
    - int (64 bit signed)
    - function
- Type inference with Hindley-Milner algorithm.

## Development

- Install .NET Core Cli Tools 2.1.
- Install F# 4.1.

In the `src` directory:

```sh
# To build:
dotnet build

# To test:
dotnet test Bracky.Runtime.Tests
```
