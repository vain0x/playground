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
        : ITextWritable
    {
        sealed class EntityClassWriter
            : CSharpWriter
        {
            EntityClass Parent { get; }

            DbTable Table => Parent.Table;

            static readonly string[] usingDirectives =
                new[]
                {
                    "System",
                    "System.ComponentModel.DataAnnotations",
                    "System.ComponentModel.DataAnnotations.Schema",
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

                if (!string.IsNullOrEmpty(column.Default))
                {
                    WriteLine("[DatabaseGenerated(DatabaseGeneratedOption.Computed)]");
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

                using (WriteNamespaceBlock(Parent.Style.Namespace))
                {
                    WriteClass();
                }
            }

            public EntityClassWriter(EntityClass parent, TextWriter writer)
                : base(writer)
            {
                Parent = parent;
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

        public EntityClass(CSharpStyle style, ITypeNameMapper typeNameMapper, DbTable table)
        {
            Style = style;
            TypeNameMapper = typeNameMapper;
            Table = table;
        }
    }
}
