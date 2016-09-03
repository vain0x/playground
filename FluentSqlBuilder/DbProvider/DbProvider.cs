using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;

namespace FluentSqlBuilder.Provider
{
    public class DbProvider
    {
        public SqlLanguage Language { get; }
        public DbProviderFactory Factory { get; }

        #region Dictionary between Type and DbType
        readonly Dictionary<DbType, Type> typeFromDbType;
        readonly Dictionary<Type, DbType> dbTypeFromType;

        /// <summary>
        /// A dictionary to Type from DbType. This mapping is one-to-one.
        /// </summary>
        public IReadOnlyDictionary<DbType, Type> TypeFromDbType => typeFromDbType;

        /// <summary>
        /// A dictionary to DbType from Type. This mapping is one-to-one.
        /// </summary>
        public IReadOnlyDictionary<Type, DbType> DbTypeFromType => dbTypeFromType;

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
            return DbTypeFromType.ToDictionary(kv => kv.Value, kv => kv.Key);
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
        #endregion

        public DbProvider(SqlLanguage language, DbProviderFactory factory)
        {
            Language = language;
            Factory = factory;
            dbTypeFromType = CreateDbTypeFromType();
            typeFromDbType = CreateTypeFromDbType();
        }
    }
}
