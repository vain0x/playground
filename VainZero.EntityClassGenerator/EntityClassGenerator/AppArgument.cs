using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using CommandLine;
using VainZero.EntityClassGenerator.Providers;

namespace VainZero.EntityClassGenerator
{
    public sealed class AppArgument
    {
        [Option('o', "output", Required = true, HelpText = "The directory where class files are written")]
        public string OutputDirectoryPath { get; set; }

        [Option('p', "provider", Required = true, HelpText = "The type of provider: mysql")]
        public DbProviderType ProviderType { get; set; }

        [Option('c', "connection-string", Required = true, HelpText = "A connection string")]
        public string ConnectionString { get; set; }

        [Option("namespace", Required = true, HelpText = "The namespace to locate classes")]
        public string Namespace { get; set; }
    }
}
