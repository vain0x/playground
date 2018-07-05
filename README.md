# Puqqing-lang

*This repository is currently at very early stage. None of features has been implemented...*

**Puqqing-lang** ~~is~~ will be purely functional and dynamic programming language. Developing interpreter works on NodeJS.

## Data Structures

Same as JavaScript, JSON is a subset.

```puqqing
    let obj = {
        foo: 1,
        bar: "a",
    };

    // Can't mutate.
    obj.foo = 2;

    // Ok.
    let obj2 = { ...obj, foo: 2 };
```

## IO

Computation is pure by default. To cause side effects, enclose the "impure" region with ``io { ... }`` block and use affect operator `!` inside of it explicitly.

```puqqing
    let main = io {
        // Get current time. Let defines an immutable variable (`const` in JavaScript).
        let now = js.Date()!

        // Send to standard output.
        js.console.log("hello world at " + now)!
    }
```

## Development

- Install NodeJS 8.9.

In the `src` directory:

```bash
# First set up.
npm install

# Run sample program.
$(npm bin)/ts-node ./core/index.ts
```
