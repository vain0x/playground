using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using VainZero.EntityClassGenerator.Domain;

namespace VainZero.EntityClassGenerator.CSharpWriters
{
    public sealed class EntityClassCollection
    {
        CSharpStyle Style { get; }
        ITypeNameMapper TypeNameMapper { get; }
        DbSchema Schema { get; }

        IEnumerable<EntityClass> EntityClasses()
        {
            foreach (var table in Schema.Tables)
            {
                yield return new EntityClass(Style, TypeNameMapper, table);
            }
        }

        public void WriteToDirectory(DirectoryInfo directory)
        {
            foreach (var entityClass in EntityClasses())
            {
                var file = new FileInfo(Path.Combine(directory.FullName, entityClass.ClassName + ".cs"));
                entityClass.WriteToFile(file);
            }
        }

        public EntityClassCollection(CSharpStyle style, ITypeNameMapper typeNameMapper, DbSchema schema)
        {
            Style = style;
            TypeNameMapper = typeNameMapper;
            Schema = schema;
        }
    }
}
