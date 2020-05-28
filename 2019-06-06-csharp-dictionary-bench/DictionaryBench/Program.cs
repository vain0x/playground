using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using BenchmarkDotNet;
using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Configs;
using BenchmarkDotNet.Jobs;

namespace DictionaryBench
{
    sealed class Entity
    {
        public long Id { get; }
        public int Value { get; }

        public Entity(long id, int value)
        {
            Id = id;
            Value = value;
        }
    }

    // NO sealed
    public class Benchmarks
    {
        static readonly int _n = 100000;

        static readonly IEnumerable<Entity> _source =
            Enumerable.Range(0, _n)
            .Select(x => new Entity((long)x, x * x % 17))
            .ToArray();

        static int Use(IDictionary<long, Entity> dictionary)
        {
            var h = 0;
            foreach (var entity in _source)
            {
                Entity found;
                if (!dictionary.TryGetValue(entity.Id, out found))
                    throw new Exception();

                if (entity != found)
                    throw new Exception();

                h ^= found.Value;
            }
            return h;
        }

        // Benchmark-hacking implementation
        [Benchmark]
        public int ToLinearMap()
        {
            return Use(_source.ToLinearMap(x => x.Id));
        }

        [Benchmark]
        public int ToDictionary()
        {
            return Use(_source.ToDictionary(x => x.Id));
        }

        [Benchmark]
        public int ToAssoc()
        {
            return Use(_source.ToAssoc(x => x.Id));
        }

        [Benchmark]
        public int ToImmutableSortedDictionary()
        {
            return Use(_source.ToImmutableSortedDictionary(x => x.Id, x => x));
        }

        [Benchmark]
        public int ToImmutableDictionary()
        {
            return Use(_source.ToImmutableDictionary(x => x.Id));
        }
    }

    sealed class Program
    {
        static void Main(string[] args)
        {
            // var rough = new AccuracyMode { MaxRelativeError = 0.1 };
            // var quickRoughJob = new Job("QuickRough", rough, RunMode.Short);
            // var config = new ManualConfig();
            // config.Add(quickRoughJob);
            // config = ManualConfig.Union(DefaultConfig.Instance, config);

            BenchmarkDotNet.Running.BenchmarkRunner.Run<Benchmarks>();
        }
    }
}
