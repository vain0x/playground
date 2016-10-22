using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DotNetKit.ErrorHandling;
using AsterSql.TypedRecord;

namespace AsterSql.Engine.Memory
{
    class MemoryColumn
    {
        public string Name { get; }
        public DbType Type { get; }

        public MemoryColumn(string name, DbType type)
        {
            Name = name;
            Type = type;
        }
    }

    class MemoryRow
        : Dictionary<string, object>
        , IValueRecord
    {
    }

    class MemoryTable
    {
        public string Name { get; }
        public IReadOnlyList<MemoryColumn> Columns { get; }
        public IEnumerable<MemoryRow> Rows { get; }

        public MemoryTable(string name, IReadOnlyList<MemoryColumn> columns, IEnumerable<MemoryRow> rows)
        {
            Name = name;
            Columns = columns;
            Rows = rows;
        }
    }
}
