using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AsterSql.Data
{
    /// <summary>
    /// Represents a one-to-one mapping between Type and DbType.
    /// </summary>
    public sealed class DbTypeMap
    {
        readonly Dictionary<DbType, Type> typeFromDbType;
        readonly Dictionary<Type, DbType> dbTypeFromType;

        /// <summary>
        /// Gets Type from DbType. This mapping is one-to-one.
        /// </summary>
        public Type TypeFromDbType(DbType dbType) => typeFromDbType[dbType];

        /// <summary>
        /// Gets DbType from Type. This mapping is one-to-one.
        /// </summary>
        public DbType DbTypeFromType(Type type) => dbTypeFromType[type];

        // TODO: Complete the list.
        Dictionary<Type, DbType> CreateDbTypeFromType()
        {
            return
                new Dictionary<Type, DbType>()
                {
                    { typeof(int), DbType.Int32 },
                    { typeof(long),DbType.Int64 },
                    { typeof(double), DbType.Double },
                    { typeof(string), DbType.String },
                    { typeof(DateTime), DbType.DateTime }
                };
        }

        Dictionary<DbType, Type> CreateTypeFromDbType()
        {
            return dbTypeFromType.ToDictionary(kv => kv.Value, kv => kv.Key);
        }

        /// <summary>
        /// Add a mapping between Type and DbType.
        /// </summary>
        /// <param name="type"></param>
        /// <param name="dbType"></param>
        public void AssociateType(Type type, DbType dbType)
        {
            typeFromDbType[dbType] = type;
            dbTypeFromType[type] = dbType;
        }

        public DbTypeMap()
        {
            dbTypeFromType = CreateDbTypeFromType();
            typeFromDbType = CreateTypeFromDbType();
        }
    }
}
