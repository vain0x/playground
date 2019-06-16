module Program

open System.Text
open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Jobs

let inline cons head tail = head :: tail

type LinkListArena<'T>() =
  let mutable arena = ResizeArray<struct (int * int * 'T)>()

  // arena.[0] を nil にする。
  do arena.Add((0, 0, Unchecked.defaultof<'T>))

  member val nil = 0

  member this.cons item tail =
    let id = arena.Count
    arena.Add((tail, this.length tail + 1, item))
    id

  member __.length tip =
    let struct (_, n, _) = arena.[tip]
    n

  member __.Item
    with get id =
      if id = 0 then
        ValueNone
      else
        let struct (tail, _, item) = arena.[id]
        ValueSome (item, tail)

  member this.toArray tip =
    let array = Array.zeroCreate (this.length tip)

    let rec go tip =
      if tip <> 0 then
        let struct (tail, length, item) = arena.[tip]
        array.[length - 1] <- item
        go tail

    go tip
    array

[<Struct>]
type Token =
  | Int
    of intValue:int
  | Str
    of strValue:string

type Benchmarks() =
  [<Benchmark>]
  member __.StringBuilder() =
    let out = StringBuilder()
    let rec go (out: StringBuilder) i =
      if i <= 10_000 then
        out
          .Append(i).Append(",")
          .Append(i * i).Append("\n")
          |> ignore
        go out (i + 1)
    go out 1
    out.ToString()

  [<Benchmark>]
  member __.StringListConcat() =
    let rec go i acc =
      if i > 10_000 then
        acc
      else
        acc
        |> cons (string i) |> cons ","
        |> cons (string (i * i)) |> cons "\n"
        |> go (i + 1)
    [] |> go 1 |> List.rev |> String.concat ""

  [<Benchmark>]
  member __.StringListConcatWithArena() =
    let arena = LinkListArena<string>()

    let rec go i acc =
      if i > 10_000 then
        acc
      else
        acc
        |> arena.cons (string i) |> arena.cons ","
        |> arena.cons (string (i * i)) |> arena.cons "\n"
        |> go (i + 1)

    arena.nil |> go 1 |> arena.toArray |> String.concat ""

  [<Benchmark>]
  member __.TokenListRender() =
    let render tokens =
      let tokens = tokens |> List.toArray
      let out = StringBuilder()

      for i in tokens.Length - 1..-1..0 do
        match tokens.[i] with
        | Token.Int value ->
          out.Append(value) |> ignore

        | Token.Str value ->
          out.Append(value) |> ignore

      out.ToString()

    let rec go i acc =
      if i > 10_000 then
        acc
      else
        acc
        |> cons (Token.Int i) |> cons (Token.Str ",")
        |> cons (Token.Int (i * i)) |> cons (Token.Str "\n")
        |> go (i + 1)

    [] |> go 1 |> render

  [<Benchmark>]
  member __.StringBuilderBad() =
    let out = StringBuilder()
    let rec go (out: StringBuilder) i =
      if i <= 10_000 then
        // NOTE: Don't do this.
        out.Append(sprintf "%d,%d\n" i (i * i)) |> ignore
        go out (i + 1)
    go out 1
    out.ToString()

  [<Benchmark>]
  member __.StringListConcatBad() =
    let rec go i acc =
      if i > 10_000 then
        acc
      else
        // NOTE: Don't do this.
        acc
        |> cons (sprintf "%d,%d\n" i (i * i))
        |> go (i + 1)
    [] |> go 1 |> List.rev |> String.concat ""

[<EntryPoint>]
let main _ =
  // 結果が正しいことを確認する。
  let expected = Benchmarks().StringBuilder()
  assert (
    Benchmarks().StringListConcat() = expected
    && Benchmarks().StringListConcatWithArena() = expected
    && Benchmarks().TokenListRender() = expected
  )

#if !DEBUG
  // ベンチマークをとる。
  let config () =
    let rough = AccuracyMode(MaxRelativeError = 0.1)
    let quickRoughJob = Job("QuickRough", rough, RunMode.Short)
    let mutable config = ManualConfig()
    config.Add(quickRoughJob)
    config <- ManualConfig.Union(DefaultConfig.Instance, config)
    config

  let _summary = Running.BenchmarkRunner.Run<Benchmarks>(config ())
#endif

  0
