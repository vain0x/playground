import { Loop } from "./loop"

const N = 5e6

const timing = <T>(title: string, action: () => T): T => {
  action() // Warming up.

  gc?.()
  const mem1 = process.memoryUsage()
  console.time(title)
  const result = action()
  console.timeEnd(title)
  const mem2 = process.memoryUsage()
  const used = (mem2.heapUsed - mem1.heapUsed) / 1000
  console.log(`${title}: ${used}KB`)
  gc?.()
  return result
}

const array = [...new Array(N).keys()]

function* rangeIterator(start: number, end: number) {
  for (let i = start; i < end; i++) yield i
}

type TimingFn = <T>(title: string, action: () => T) => T

const bench = (timing: TimingFn) => {
  let sum = 0

  sum ^= timing("native loop", () => {
    let n = 0
    for (let i = 0; i < N; i++) {
      n ^= i + 1
    }
    return n
  })

  sum ^= timing("iterator of array loop", () => {
    let n = 0
    for (const i of array) {
      n ^= i + 1
    }
    return n
  })

  sum ^= timing("iterator of generator loop", () => {
    let n = 0

    for (const i of rangeIterator(0, N)) {
      n ^= i + 1
    }
    return n
  })

  sum ^= timing("loop", () => {
    let n = 0
    Loop.range(0, N).map(i => i + 1).forEach(i => {
      n ^= i
    })
    return n
  })

  return sum & 0x7f
}

bench((_, action) => action())
process.exit(bench(timing))
