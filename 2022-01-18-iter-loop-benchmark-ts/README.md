# Iterator vs. Loop benchmark

Micro benchmark.

*My conclusion at this time: I do NOT use Loop. Loop might outperform Iterator. However, difference is little.*

## Usage

```sh
npm ci
npm run
```

## Motivation

- Iterator interface produces garbages in the order of iteration. Iterator seems inefficient due to GC pressure.
- Most of iteration can be written with more efficient abstraction, named `Loop` here.

## What is Loop

`Loop` is an abstraction of an iteration.

Technically `Loop` can be a function that calls a given action repeatedly.

```ts
// Simplified definition.
type Loop<T> = (action: (item: T) => void) => void
```

As Iterator does, `Loop` can represent an iteration over integers in a range.

```ts
const range = (start: number, end: number): Loop<T> =>
    (action: (item: T) => void) => {
        for (let i = start; i < end; i++) {
            action(i)
        }
    }
```

Usage:

```ts
    const loop = range(0, 3)
    loop(i => { console.log(i) })
    // prints 0, 1, 2
```

Make sure that no object is allocated in the body of iteration. Closures are allocated outside of iteration. That's the reason `Loop` makes garbages less than Iterator.

See [src/loop.ts](src/loop.ts) for implementation. `Loop` is defined as a class there for method-chain notation.

### Limitation

`Loop` is NOT a drop-in replacement of Iterator. For example, Loop can't support `zip` in the meaning of Iterator.

`Loop` can support basic operations: map, filter, reduce, forEach, toArray etc. You could quote the 80-20 rule to justify it.

"I don't mind to write `for` loop in rare situations where `Loop` doesn't work. Application performs better by replacing Iterator with Loop with little loss of productivity."

## Result

On my machine.

Each case prints two lines: time and heap memory delta.

```
native loop: 3.735ms
native loop: 362.152KB
iterator of array loop: 8.406ms
iterator of array loop: 274.472KB
iterator of generator loop: 97.844ms
iterator of generator loop: 2315.528KB
loop: 14.895ms
loop: 482.024KB
```

- Native loop (`for`) is efficient. That's baseline.
- Iterator of array (`for (const _ of array)`) is also efficient enough. 2x slower than baseline.
- Iterator of generator (`for (const _ of generator())`) is inefficient. 20x slower and 10x garbages.
- Loop is efficient. 5x slower and almost-zero garbages compared to baseline.

The number of iteration is unrealistically large in the benchmark for measurement.
