module rec FSharpBench.MapBench

open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open FSharpBench.MapBenchAssoc

type MapBenchmarks() =
  let n = 100000

  [<Benchmark>]
  member __.Map() =
    let mutable map = Map.empty
    for i in 0..n - 1 do
      map <- map |> Map.add i (i + 1)

    let mutable h = 17
    for i in 0..n * 2 - 1 do
      h <- h ^^^ (map |> Map.tryFind i |> Option.defaultValue i)

    h

  [<Benchmark>]
  member __.AssocMap() =
    let mutable map = mapEmpty (intHash, intCmp)

    for i in 0..n - 1 do
      map <- map |> mapAdd i (i + 1)

    let mutable h = 17
    for i in 0..n * 2 - 1 do
      h <- h ^^^ (map |> mapTryFind i |> Option.defaultValue i)

    h

let verify () =
  let bench = MapBenchmarks()
  let expected = bench.Map()

  assert (bench.AssocMap() = expected)
