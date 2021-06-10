# Aceml

WIP

Aceml is an HTML-like markup language
with brace-oriented syntax.

## Examples

```aceml
{h1: Aceml}

{p: WIP}

{h2: Examples}

{p: Aceml is an HTML-like markup language
    with brace-oriented syntax.}

{pre {lang:aceml}: ```
    {h1: Aceml}...
```}

See also {a {href:./examples}: examples}.
```

See also [examples](./examples).

## TODO: Usage/Install/etc.

----
----

## Spec

## Nodes

### Elements

Generic form:

```
{tag attr: body}
```

Attributes (attr) may contain both text nodes and another elements.
`{lang:en}` means `lang=en` in HTML.

Short form (body can't include `:`s):

```
{body}
```

## Text nodes

- Verbatim:
    Sequence of non-space, non-meta characters such as `hello`, `http-equiv`, `"Yay!"` etc.
    - Meta characters are `{`, `}`, `:`, `\` and `` ` ``.
    - `:` may appear in verbatim depending on the context.
- Spaces:
    - Leading/trailing spaces of element body are trimmed.
    - Between nodes, whitespaces are preserved.
    - `\` followed by immediate whitespace or newline removes the further spaces.
- Escaped:
    - `\xHH`
    - `\u{HHHH}`
- Backtick:
    - `` ``` ... ``` `` for arbitrary text (excluding triple-backticks.)

----
----

## Prior arts

- [kqr/braceml](https://github.com/kqr/braceml)
    (and <https://orgmode.org/org.html>?) (2017-08-24)
    (`<strong>INLINE</strong>` → `{/ INLINE /}` etc.)
- [Suggestion for a Better XML/HTML Syntax](https://www.codeproject.com/Articles/5296768/Suggestion-for-a-Better-XML-HTML-Syntax) (2021-04-20)
    (`<n>v</n>` → `[n v]`)
