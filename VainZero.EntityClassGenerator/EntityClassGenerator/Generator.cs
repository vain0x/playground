using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using VainZero.EntityClassGenerator.CSharpWriters;
using VainZero.EntityClassGenerator.Providers;

namespace VainZero.EntityClassGenerator
{
    public sealed class Generator
    {
        AppArgument Argument { get; }

        IDbProvider Provider()
        {
            switch (Argument.ProviderType)
            {
                case DbProviderType.MySql:
                    return new MySqlDbProvider(Argument.ConnectionString);
                default:
                    throw new NotSupportedException("Specify valid provider.");
            }
        }

        public void Generate()
        {
            var style = new CSharpStyle(Argument.Namespace);
            var provider = Provider();
            var schema = provider.GetSchema();
            var collection = new EntityClassCollection(style, provider.TypeNameMapper, schema);
            var outputDirectory = new DirectoryInfo(Argument.OutputDirectoryPath);
            collection.WriteToDirectory(outputDirectory);
        }

        public Generator(AppArgument argument)
        {
            Argument = argument;
        }
    }
}
