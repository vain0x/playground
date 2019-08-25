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

|                    Method |       Mean |     Error |    StdDev |
|-------------------------- |-----------:|----------:|----------:|
|             StringBuilder |   872.9 us |  73.57 us |  4.033 us |
|          StringListConcat | 4,787.9 us | 706.28 us | 38.714 us |
| StringListConcatWithArena | 5,130.2 us | 767.93 us | 42.093 us |
|           TokenListRender | 2,463.3 us | 431.33 us | 23.642 us |
|  TokenListRenderWithArena | 2,758.9 us | 456.94 us | 25.046 us |
|          StringBuilderBad | 3,080.6 us | 534.22 us | 29.282 us |
|       StringListConcatBad | 5,051.6 us | 509.06 us | 27.903 us |
