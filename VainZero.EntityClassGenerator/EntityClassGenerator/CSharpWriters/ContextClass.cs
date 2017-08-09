using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using VainZero.EntityClassGenerator.Domain;

namespace VainZero.EntityClassGenerator.CSharpWriters
{
    public sealed class ContextClass
        : ITextWritable
    {
        sealed class ContextClassWriter
            : CSharpWriter
        {
            ContextClass Parent { get; }

            string ClassName => Parent.Style.ContextName;

            static readonly string[] usingDirectives =
                new[]
                {
                    "System",
                    "System.Data.Common",
                    "System.Data.Entity",
                    "System.Data.Entity.Core.Objects",
                    "System.Data.Entity.Infrastructure",
                }
                .Select(namespaceName => $"using {namespaceName};")
                .ToArray();

            public void WriteUsings()
            {
                foreach (var directive in usingDirectives)
                {
                    WriteLine(directive);
                }
            }

            IDisposable WriteClassBlock()
            {
                WriteLine($"public partial class {ClassName}");

                using (AddIndent())
                {
                    WriteLine(": DbContext");
                }

                return WriteBlock();
            }

            void WriteDerivedConstructor(string modifier, params (string typeName, string parameterName)[] parameters)
            {
                var parameterList =
                    string.Join(", ", parameters.Select(t => $"{t.typeName} {t.parameterName}"));
                WriteLine($"{modifier} {ClassName}({parameterList})");

                if (parameterList.Length > 0)
                {
                    using (AddIndent())
                    {
                        var list = string.Join(", ", parameters.Select(t => t.parameterName));
                        WriteLine($": base({list})");
                    }
                }

                using (WriteBlock())
                {
                }

                WriteLine("");
            }

            void WriteConstructors()
            {
                WriteDerivedConstructor("protected");

                WriteDerivedConstructor("protected", ("DbCompiledModel", "model"));

                WriteDerivedConstructor("public", ("string", "nameOrConnectionString"));

                WriteDerivedConstructor(
                    "public",
                    ("string", "nameOrConnectionString"),
                    ("DbCompiledModel", "model")
                );

                WriteDerivedConstructor(
                    "public",
                    ("DbConnection", "connection"),
                    ("bool", "contextOwnsConnection")
                );

                WriteDerivedConstructor(
                    "public",
                    ("DbConnection", "existingConnection"),
                    ("DbCompiledModel", "model"),
                    ("bool", "contextOwnsConnection")
                );

                WriteDerivedConstructor(
                    "public",
                    ("ObjectContext", "objectContext"),
                    ("bool", "dbContextOwnsObjectContext")
                );
            }

            void WriteTableProperty(DbTable table)
            {
                WriteLine($"public DbSet<{table.Name}> {EscapeIdentifier(table.Name)} {{ get; set; }}");
            }

            void WriteClassContent()
            {
                WriteConstructors();

                foreach (var table in Parent.Schema.Tables)
                {
                    WriteTableProperty(table);
                }
            }

            public void Write()
            {
                WriteUsings();
                WriteLine("");

                using (WriteNamespaceBlock(Parent.Style.Namespace))
                {
                    using (WriteClassBlock())
                    {
                        WriteClassContent();
                    }
                }
            }

            public ContextClassWriter(ContextClass parent, TextWriter writer)
                : base(writer)
            {
                Parent = parent;
            }
        }

        DbSchema Schema { get; }

        CSharpStyle Style { get; }

        public void Write(TextWriter writer)
        {
            new ContextClassWriter(this, writer).Write();
        }

        public ContextClass(DbSchema schema, CSharpStyle style)
        {
            Schema = schema;
            Style = style;
        }
    }
}
