using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using CommandLine;
using CommandLine.Text;
using VainZero.EntityClassGenerator.CSharpWriters;
using VainZero.EntityClassGenerator.Providers;

namespace VainZero.EntityClassGenerator
{
    public sealed class Program
    {
        readonly string[] args;
        readonly TextWriter writer;
        readonly int width;

        bool TryParse(out AppArgument argument)
        {
            var parser = new Parser(s =>
            {
                s.ParsingCulture = CultureInfo.InvariantCulture;
                s.CaseInsensitiveEnumValues = true;
                s.HelpWriter = writer;
                s.MaximumDisplayWidth = width;
            });

            var resultBase = parser.ParseArguments<AppArgument>(args);
            switch (resultBase.Tag)
            {
                case ParserResultType.Parsed:
                    {
                        var result = (Parsed<AppArgument>)resultBase;
                        argument = result.Value;
                        return true;
                    }
                default:
                    {
                        var result = (NotParsed<AppArgument>)resultBase;
                        writer.WriteLine(HelpText.AutoBuild(result, width));
                        argument = default(AppArgument);
                        return false;
                    }
            }
        }

        void Run()
        {
            if (TryParse(out var argument))
            {
                new Generator(argument).Generate();
                return;
            }

#if DEBUG
            argument =
                new AppArgument()
                {
                    ProviderType = DbProviderType.MySql,
                    ConnectionString =
                        new MySql.Data.MySqlClient.MySqlConnectionStringBuilder()
                        {
                            Server = "localhost",
                            Database = "florida",
                            UserID = "root",
                            Password = "root",
                        }.ToString(),
                    OutputDirectoryPath = "Entities",
                    Namespace = "VainZero.Florida",
                };
            new Generator(argument).Generate();
#endif
        }

        public Program(string[] args, TextWriter writer, int width)
        {
            this.args = args;
            this.writer = writer;
            this.width = width;
        }

        public static void Main(string[] args)
        {
            new Program(args, Console.Out, Console.WindowWidth).Run();
        }
    }
}
