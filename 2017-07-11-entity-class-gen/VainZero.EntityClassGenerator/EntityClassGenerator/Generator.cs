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

        static void Clear(DirectoryInfo directory)
        {
            var retryCount = 5;

            while (true)
            {
                try
                {
                    if (!Directory.Exists(directory.FullName))
                    {
                        directory.Create();
                    }

                    foreach (var file in directory.GetFiles("*.cs"))
                    {
                        file.Delete();
                    }

                    break;
                }
                catch (IOException)
                {
                    if (retryCount > 0)
                    {
                        retryCount--;
                        continue;
                    }
                    throw;
                }
            }
        }

        public void Generate()
        {
            var style = new CSharpStyle(Argument.Namespace, Argument.ContextName);
            var provider = Provider();
            var schema = provider.GetSchema();
            var contextClass = new ContextClass(schema, style);
            var collection = new EntityClassCollection(style, provider.TypeNameMapper, schema);

            var outputDirectory = new DirectoryInfo(Argument.OutputDirectoryPath);
            Clear(outputDirectory);
            contextClass.WriteToFile(
                new FileInfo(Path.Combine(outputDirectory.FullName, style.ContextName + ".cs"))
            );
            collection.WriteToDirectory(outputDirectory);
        }

        public Generator(AppArgument argument)
        {
            Argument = argument;
        }
    }
}
