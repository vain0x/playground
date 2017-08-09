#if !DEBUG
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Configs;
using BenchmarkDotNet.Diagnosers;
using BenchmarkDotNet.Exporters;
using BenchmarkDotNet.Jobs;
using BenchmarkDotNet.Loggers;
using BenchmarkDotNet.Running;

namespace VainZero.Sandbox
{
    public class BenchmarkConfig
        : ManualConfig
    {
        public BenchmarkConfig()
        {
            Add(MarkdownExporter.GitHub);
            Add(MemoryDiagnoser.Default);

            Add(Job.ShortRun.WithTargetCount(1).WithWarmupCount(1));
        }
    }

    [Config(typeof(BenchmarkConfig))]
    public class BenchmarkMethods
    {
        [Benchmark]
        public string Greet() => "Hello, world!";
    }

    public sealed class Program
    {
        public void Run()
        {
            var summary = BenchmarkRunner.Run<BenchmarkMethods>();

            Console.WriteLine("Result:");
            MarkdownExporter.Console.ExportToLog(summary, ConsoleLogger.Default);
        }

        public static void Main(string[] args)
        {
            new Program().Run();
        }
    }
}
#endif
