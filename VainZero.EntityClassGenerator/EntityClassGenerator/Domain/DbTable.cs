using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.EntityClassGenerator.Domain
{
    public sealed class DbTable
    {
        public string Name { get; }
        public IReadOnlyList<DbColumn> Columns { get; }
        public IReadOnlyDictionary<DbColumn, int> KeyColumnOrder { get; }

        public DbTable(string name, IReadOnlyList<DbColumn> columns)
        {
            Name = name;
            Columns = columns;

            KeyColumnOrder =
                columns
                .Where(column => column.IsPrimaryKey)
                .Select((c, i) => new KeyValuePair<DbColumn, int>(c, i))
                .ToDictionary(kv => kv.Key, kv => kv.Value);
        }
    }
}
