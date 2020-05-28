module FSharpBench.Program

open BenchmarkDotNet
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Jobs

let config () =
  let rough = AccuracyMode(MaxRelativeError = 0.1)
  let quickRoughJob = Job("QuickRough", rough, RunMode.Short)
  let mutable config = ManualConfig()
  config.Add(quickRoughJob)
  config <- ManualConfig.Union(DefaultConfig.Instance, config)
  config

type Benchmarks () =
  // inherit StringConcatBench.StringConcatBenchmarks()
  inherit MapBench.MapBenchmarks()

let verify () =
  StringConcatBench.verify ()
  MapBench.verify ()

let runBenchmark () =
  Running.BenchmarkRunner.Run<Benchmarks>(config ()) |> ignore

[<EntryPoint>]
let main _ =
#if DEBUG
  verify ()
#else
  runBenchmark ()
#endif
  0
