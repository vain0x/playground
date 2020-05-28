module FSharpBench.StringConcatBench

open System.Text
open BenchmarkDotNet
open BenchmarkDotNet.Attributes

let inline cons head tail = head :: tail

type LinkListArena<'T>(capacity: int) =
  let mutable tails = ResizeArray<int>(1 + capacity)
  let mutable items = ResizeArray<'T>(1 + capacity)

  member val nil =
    tails.Add(-1)
    items.Add(Unchecked.defaultof<'T>)
    tails.Count - 1

  member __.cons item tail =
    tails.Add(tail)
    items.Add(item)
    tails.Count - 1

  member this.length tip =
    let rec go acc tip =
      if tip = this.nil then
        acc
      else
        go (acc + 1) tails.[tip]
    go 0 tip

  member this.toArray tip =
    let length = this.length tip
    let array = Array.zeroCreate length

    let rec go length tip =
      if tip = this.nil then
        assert (length = 0)
      else
        array.[length - 1] <- items.[tip]
        go (length - 1) tails.[tip]

    go length tip
    array

[<Struct>]
type Token =
  | Int
    of intValue:int
  | Str
    of strValue:string

type StringConcatBenchmarks() =
  [<Benchmark>]
  member __.StringBuilder() =
    let out = StringBuilder(4096)
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
    let arena = LinkListArena<string>(4096)

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
      let out = StringBuilder(4096)

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
  member __.TokenListRenderWithArena() =
    let arena = LinkListArena<Token>(4096)

    let render tokens =
      let tokens = tokens |> arena.toArray
      let out = StringBuilder(4096)

      for t in tokens do
        match t with
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
        |> arena.cons (Token.Int i) |> arena.cons (Token.Str ",")
        |> arena.cons (Token.Int (i * i)) |> arena.cons (Token.Str "\n")
        |> go (i + 1)

    arena.nil |> go 1 |> render

  [<Benchmark>]
  member __.StringBuilderBad() =
    let out = StringBuilder(4096)
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

let verify () =
  // 結果が正しいことを確認する。
  let bench = StringConcatBenchmarks()
  let expected = bench.StringBuilder()

  assert (bench.StringListConcat() = expected)
  assert (bench.StringListConcatWithArena() = expected)
  assert (bench.TokenListRender() = expected)
  assert (bench.TokenListRenderWithArena() = expected)
  assert (bench.StringBuilderBad() = expected)
  assert (bench.StringListConcatBad() = expected)
