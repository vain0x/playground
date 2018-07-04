# Bracky-lang

*This repository is currently at very early stage and inactive.*

**Bracky-lang** is a programming language with *humorous* syntax in the middle of ML and LISP.

## Syntax

The code looks like the following:

```fsharp
// Bracky code. (Highlighted as F# though.)

{ val main () =
  { if true ->
      {val x = 1};
      x + 2;
    if false -> 3;
    else 4
  }
}
```

Because `if` expressions occur directly inside of `{}`, it is distinct from other `if`s that mean `else-if` in the same block. So all conditions are aligned at the same column.

## Semantics

- Types:
    - unit
    - bool
    - int (64 bit signed)
    - function
- Type inference with Hindly-Milner algorithm.

## Development

- Install .NET Core CLI 2.1.
- Install F# 4.1.

In `./src` directory:

```sh
# To build:
dotnet build

# To test:
dotnet test Bracky.Runtime.Tests
```
