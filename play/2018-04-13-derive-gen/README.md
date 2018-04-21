# DeriveGen

CLI app to generate C# boilerplate codes.

## Features

Generate C# struct/class definition that includes:

- Complete constructor,
- Structural equality support,
- ToString override,

etc.

## Usage

```sh
cat input.json | derive-gen.exe > output.cs
```

See also `examples` directory.

### Input Json File

```json
{
  "types": {
    "<namespace>": {
      "<class-name>": {
        "kind": "<type-kind>",
        "derive": ["<derive-name>", ...],
        "fields": {
          "<field-name>": "<field-type>"
        }
      }
    }
  }
}
```

where

- **type-kind**:
    `"class"` (default) or `"struct"`.
- **derive-name**:
    One of the following names.

    - **Eq**: Overrides `Equals` and `GetHashCode` and supports operator `==`/`!=`.
    - **ToString**: Overrides `ToString`.

    Note that `Eq` and `ToString` are included implicitly. If you don't need them, specify `"?Eq"` or `"?ToString"`.

## Known issues

- `GetHashCode` works badly.
- `ToString` works badly.
- Emits public fields.

## Related projects

- RecordConstructorGenerator:

    Analyzer/CodeFix to generate complete constructor.

- LanguageExt:

    Library provindg a way to generate code for structural equality etc. dynamically.

- Sharperform:

    App to generate such codes by analyzing C# source code with some attributes.

## See also

- [partial (Type) (C# Reference)](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/partial-type):

    Document of `partial class`.

- [Champion "Records" #39](https://github.com/dotnet/csharplang/issues/39):

    Work to add Record type to C# (>= 8).
