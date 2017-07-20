using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace VainZero.EntityClassGenerator.Domain
{
    public sealed class DbColumn
    {
        public string TableName { get; }
        public string Name { get; }
        public bool IsNullable { get; }
        public string TypeName { get; }
        public int? MaxLength { get; }
        public int? NumericPrecision { get; }
        public int? NumericScale { get; }
        public bool IsPrimaryKey { get; }
        public bool IsAutoIncrement { get; }
        public int Priority { get; }

        public DbColumn(string tableName, string name, bool isNullable, string typeName, int? maxLength, int? numericPrecision, int? numericScale, bool isPrimaryKey, bool isAutoIncrement, int priority)
        {
            TableName = tableName;
            Name = name;
            IsNullable = isNullable;
            TypeName = typeName;
            MaxLength = maxLength;
            NumericPrecision = numericPrecision;
            NumericScale = numericScale;
            IsPrimaryKey = isPrimaryKey;
            IsAutoIncrement = isAutoIncrement;
            Priority = priority;
        }
    }
}
