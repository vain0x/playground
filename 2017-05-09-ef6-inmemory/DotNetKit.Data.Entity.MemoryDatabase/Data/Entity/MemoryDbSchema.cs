using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DotNetKit.Data.Entity
{
    /// <summary>
    /// Represents a database schema which works in-memory.
    /// </summary>
    public sealed class MemoryDbSchema
        : IDbSchema
    {
        Dictionary<Type, IMemoryDbTable> Tables { get; } =
            new Dictionary<Type, IMemoryDbTable>();

        IMemoryDbTable GetOrAddTableCore(Type entityType)
        {
            {
                var table = default(IMemoryDbTable);
                if (Tables.TryGetValue(entityType, out table))
                {
                    return table;
                }
            }

            {
                var table = MemoryDbTableModule.Create(entityType);
                Tables.Add(entityType, table);
                return table;
            }
        }

        internal IMemoryDbTable GetOrAddTable<E>()
            where E : class
        {
            return GetOrAddTableCore(typeof(E));
        }

        /// <summary>
        /// Connects the schema.
        /// </summary>
        /// <returns></returns>
        public DbContext Connect()
        {
            return new MemoryDbContext(this);
        }
    }
}
