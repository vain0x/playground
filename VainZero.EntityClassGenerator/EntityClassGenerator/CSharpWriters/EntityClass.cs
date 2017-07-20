using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using VainZero.EntityClassGenerator.Domain;
using VainZero.Misc.Disposables;

namespace VainZero.EntityClassGenerator.CSharpWriters
{
    public sealed class EntityClass
    {
        sealed class EntityClassWriter
        {
            sealed class Indentation
                : IDisposable
            {
                EntityClassWriter Parent { get; }

                public Indentation(EntityClassWriter parent)
                {
                    Parent = parent;

                    Parent.indentLevel++;
                }

                public void Dispose()
                {
                    Parent.indentLevel--;
                }
            }

            EntityClass Parent { get; }
            TextWriter Writer { get; }

            DbTable Table => Parent.Table;

            int indentLevel;

            static IDisposable Defer(Action action)
            {
                return new AnonymousDisposable(action);
            }

            Indentation AddIndent()
            {
                return new Indentation(this);
            }

            void WriteIndent()
            {
                for (var i = 0; i < indentLevel * 4; i++)
                {
                    Writer.Write(' ');
                }
            }

            void WriteLine(string line)
            {
                if (string.IsNullOrWhiteSpace(line))
                {
                    Writer.WriteLine();
                    return;
                }

                WriteIndent();
                Writer.WriteLine(line);
            }

            IDisposable WriteBlock()
            {
                WriteLine("{");
                var indentation = AddIndent();
                return
                    Defer(() =>
                    {
                        indentation.Dispose();
                        WriteLine("}");
                    });
            }

            IDisposable WriteNamespaceBlock()
            {
                WriteLine($"namespace {Parent.Style.Namespace}");
                return WriteBlock();
            }

            static readonly string[] usingDirectives =
                new[]
                {
                    "System",
                    "System.ComponentModel.DataAnnotations",
                    "System.ComponentModel.DataAnnotations.Schema",
                }
                .Select(namespaceName => $"using {namespaceName};")
                .ToArray();

            void WriteUsings()
            {
                foreach (var directive in usingDirectives)
                {
                    WriteLine(directive);
                }
            }

            static string EscapeIdentifier(string name)
            {
                return new CSharpIdentifier(name).Name;
            }

            IDisposable WriteClassBlock()
            {
                WriteLine($"[Table(\"{Table.Name}\")]");
                WriteLine($"public partial class {EscapeIdentifier(Parent.ClassName)}");
                return WriteBlock();
            }

            void WriteProperty(DbColumn column)
            {
                var typeName = Parent.TypeNameMapper.CSharpTypeName(column);

                if (column.IsPrimaryKey)
                {
                    WriteLine("[Key]");
                }

                if (Table.KeyColumnOrder.Count >= 2 && Table.KeyColumnOrder.TryGetValue(column, out int columnIndex))
                {
                    WriteLine($"[Column(\"{column.Name}\", Order = {columnIndex})]");
                }
                else
                {
                    WriteLine($"[Column(\"{column.Name}\")]");
                }

                if (typeName == "string" && column.MaxLength.HasValue && column.MaxLength.Value >= 0)
                {
                    WriteLine($"[StringLength({column.MaxLength.Value})]");
                }

                WriteLine($"public {typeName} {EscapeIdentifier(column.Name)} {{ get; set; }}");
            }

            void WriteClass()
            {
                using (WriteClassBlock())
                {
                    for (var i = 0; i < Table.Columns.Count; i++)
                    {
                        if (i != 0) WriteLine("");
                        WriteProperty(Table.Columns[i]);
                    }
                }
            }

            public void WriteFile()
            {
                WriteUsings();
                WriteLine("");

                using (WriteNamespaceBlock())
                {
                    WriteClass();
                }
            }

            public EntityClassWriter(EntityClass parent, TextWriter writer)
            {
                Parent = parent;
                Writer = writer;
            }
        }

        CSharpStyle Style { get; }
        ITypeNameMapper TypeNameMapper { get; }
        DbTable Table { get; }

        public string ClassName => Table.Name;

        public override string ToString()
        {
            return ClassName;
        }

        public void Write(TextWriter writer)
        {
            new EntityClassWriter(this, writer).WriteFile();
        }

        public void WriteToFile(FileInfo file)
        {
            using (var stream = file.OpenWrite())
            using (var writer = new StreamWriter(stream))
            {
                stream.SetLength(0);
                Write(writer);
            }
        }

        public string GetString()
        {
            using (var writer = new StringWriter())
            {
                Write(writer);
                return writer.ToString();
            }
        }

        public EntityClass(CSharpStyle style, ITypeNameMapper typeNameMapper, DbTable table)
        {
            Style = style;
            TypeNameMapper = typeNameMapper;
            Table = table;
        }
    }
}
