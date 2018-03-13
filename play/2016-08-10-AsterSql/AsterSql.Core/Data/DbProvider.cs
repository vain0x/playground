using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;

namespace AsterSql.Data
{
    public class DbProvider
    {
        public SqlLanguage Language { get; }
        public DbProviderFactory Factory { get; }
        public DbTypeMap DbTypeMap { get; }

        public DbProvider(SqlLanguage language, DbProviderFactory factory)
        {
            Language = language;
            Factory = factory;
            DbTypeMap = new DbTypeMap();
        }
    }
}
