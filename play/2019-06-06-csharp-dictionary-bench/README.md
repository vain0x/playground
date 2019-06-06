# Dictionary Benchmark

Benchmark of dictionaries in a specific use case.

## Use Case

- There is an N-length array
- Create a dictionary from the array
- Iterate over the array and search the dictionary for an item

## Classes

- ToLinearMap: Benchmark-hacking array of key-value pairs. It assumes the client should access to in order and never use non-existing keys
- ToDictionary: Use `ToDictionary`
- ToAssoc: Use `ToDictionary` and wrap it with immutable wrapper
- ToImmutableSortedDictionary: From `System.Collections.Immutable`
- ToImmutableDictionary: Same

## Result

N=100000 (10^5)

``` ini
BenchmarkDotNet=v0.11.5, OS=ubuntu 18.04
Intel Core i3-7100 CPU 3.90GHz (Kaby Lake), 1 CPU, 4 logical and 2 physical cores
.NET Core SDK=2.1.700
  [Host]     : .NET Core 2.1.11 (CoreCLR 4.6.27617.04, CoreFX 4.6.27617.02), 64bit RyuJIT
  DefaultJob : .NET Core 2.1.11 (CoreCLR 4.6.27617.04, CoreFX 4.6.27617.02), 64bit RyuJIT
```

|                      Method |      Mean |     Error |    StdDev |
|---------------------------- |----------:|----------:|----------:|
|                 ToLinearMap |  3.330 ms | 0.0659 ms | 0.0809 ms |
|                ToDictionary |  3.068 ms | 0.0601 ms | 0.1129 ms |
|                     ToAssoc |  3.201 ms | 0.0633 ms | 0.0778 ms |
| ToImmutableSortedDictionary | 71.154 ms | 0.3447 ms | 0.3224 ms |
|       ToImmutableDictionary | 88.966 ms | 0.6684 ms | 0.5582 ms |
