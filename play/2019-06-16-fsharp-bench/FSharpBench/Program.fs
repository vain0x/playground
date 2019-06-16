module Program

open System.Text
open BenchmarkDotNet
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Jobs

let inline cons head tail = head :: tail

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
    for i in 1..10_000 do
      out
        .Append(i).Append(",")
        .Append(i * i).Append("\n")
        |> ignore
    out.ToString()

  [<Benchmark>]
  member __.StringListConcat() =
    let rec go i acc =
      if i > 10_000 then
        acc
        |> List.rev
        |> String.concat ""
      else
        acc
        |> cons (string i) |> cons ","
        |> cons (string (i * i)) |> cons "\n"
        |> go (i + 1)
    [] |> go 1

  [<Benchmark>]
  member __.TokenListRender() =
    let rec render (out: StringBuilder) tokens =
      match tokens with
      | [] ->
        out.ToString()

      | token :: tokens ->
        match token with
        | Token.Int value ->
          out.Append(value) |> ignore

        | Token.Str value ->
          out.Append(value) |> ignore

        render out tokens

    let rec go i acc =
      if i > 10_000 then
        acc
      else
        acc
        |> cons (Token.Int i) |> cons (Token.Str ",")
        |> cons (Token.Int (i * i)) |> cons (Token.Str "\n")
        |> go (i + 1)

    [] |> go 1 |> List.rev |> render (StringBuilder())

[<EntryPoint>]
let main _ =
  // 結果が正しいことを確認する。
  assert (
    Benchmarks().StringBuilder()
      = Benchmarks().StringListConcat()
    && Benchmarks().StringBuilder()
      = Benchmarks().TokenListRender())

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
