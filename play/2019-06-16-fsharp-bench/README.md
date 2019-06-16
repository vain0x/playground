# F# Benchmarks

F# に関するベンチマークを取る。

## 文字列連結: StringBuilder vs. string list

- StringBuilder: 数値や文字列を StringBuilder に順次 Append する
- StringListConcat: 文字列のリストを逆順で構築して、反転してから Join する
- StringListConcatWithArena: 上と同じ、ただし cons セルをオブジェクトとして生成しない
- TokenListRender: トークン (数値または文字列) のリストを逆順で構築して、反転してから StringBuilder に Append する

``` ini
BenchmarkDotNet=v0.11.5, OS=ubuntu 18.04
Intel Core i3-7100 CPU 3.90GHz (Kaby Lake), 1 CPU, 4 logical and 2 physical cores
.NET Core SDK=2.1.700
  [Host]     : .NET Core 2.1.11 (CoreCLR 4.6.27617.04, CoreFX 4.6.27617.02), 64bit RyuJIT DEBUG
  QuickRough : .NET Core 2.1.11 (CoreCLR 4.6.27617.04, CoreFX 4.6.27617.02), 64bit RyuJIT

Job=QuickRough  MaxRelativeError=0.1  IterationCount=3
LaunchCount=1  WarmupCount=3
```

|                    Method |       Mean |       Error |     StdDev |
|-------------------------- |-----------:|------------:|-----------:|
|             StringBuilder |   838.1 us |    36.59 us |   2.006 us |
|          StringListConcat | 4,386.5 us |   690.49 us |  37.848 us |
| StringListConcatWithArena | 4,757.1 us | 2,796.84 us | 153.304 us |
|           TokenListRender | 2,306.6 us |   415.55 us |  22.778 us |
