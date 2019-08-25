# F# Benchmarks

F# に関するベンチマークを取る。

## 文字列連結: StringBuilder vs. string list

- StringBuilder: 数値や文字列を StringBuilder に順次 Append する
- StringListConcat: 文字列のリストを逆順で構築して、反転してから Join する
- TokenListRender: トークン (数値または文字列) のリストを逆順で構築して、反転してから StringBuilder に Append する
- FooWithArena: cons セルをオブジェクトとして生成しない
- FooBad: 文字列の一部を sprintf で構築する

``` ini
BenchmarkDotNet=v0.11.5, OS=ubuntu 18.04
Intel Core i3-7100 CPU 3.90GHz (Kaby Lake), 1 CPU, 4 logical and 2 physical cores
.NET Core SDK=2.1.801
  [Host]     : .NET Core 2.1.12 (CoreCLR 4.6.27817.01, CoreFX 4.6.27818.01), 64bit RyuJIT DEBUG
  QuickRough : .NET Core 2.1.12 (CoreCLR 4.6.27817.01, CoreFX 4.6.27818.01), 64bit RyuJIT

Job=QuickRough  MaxRelativeError=0.1  IterationCount=3
LaunchCount=1  WarmupCount=3
```

|                    Method |       Mean |       Error |    StdDev |
|-------------------------- |-----------:|------------:|----------:|
|             StringBuilder |   827.1 us |    89.45 us |  4.903 us |
|          StringListConcat | 4,320.2 us |   952.15 us | 52.190 us |
| StringListConcatWithArena | 4,820.9 us |   450.03 us | 24.668 us |
|           TokenListRender | 2,281.3 us |   700.77 us | 38.412 us |
|  TokenListRenderWithArena | 1,914.7 us |    57.61 us |  3.158 us |
|          StringBuilderBad | 2,806.1 us | 1,041.31 us | 57.078 us |
|       StringListConcatBad | 4,481.5 us |   124.96 us |  6.850 us |
