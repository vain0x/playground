using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public class Column<TValue>
        : SqlExpression<IScalar<TValue>>
        , IColumn<TValue>
    {
        #region IColumn
        public string QualifiedName { get; }
        public string UniqueName { get; }
        public string RawName { get; }

        public DbType DbType
        {
            get
            {
                return SqlBuilder.Provider.DbTypeFromType[typeof(TValue)];
            }
        }

        public TValue this[DataRow row]
        {
            get
            {
                return row.Field<TValue>(UniqueName);
            }

            set
            {
                row.SetField(UniqueName, value);
            }
        }

        public TValue this[IValueRecord record]
        {
            get
            {
                return (TValue)record[UniqueName];
            }
            set
            {
                record[UniqueName] = value;
            }
        }

        public ISqlExpression<IScalar> this[IExpressionRecord record]
        {
            get
            {
                return record[UniqueName];
            }
            set
            {
                record[UniqueName] = value;
            }
        }

        #region SqlExpression
        public override IEnumerable<string> Tokens
        {
            get
            {
                yield return QualifiedName;
            }
        }

        public override IEnumerable<DbParameter> Parameters =>
            Enumerable.Empty<DbParameter>();
        #endregion
        #endregion

        public Column(SqlBuilder sqlBuilder, Table table, string rawName)
            : base(sqlBuilder)
        {
            RawName = rawName;
            UniqueName = SqlBuilder.Language.ConcatIdentifiers(table.Alias, RawName);
            QualifiedName = sqlBuilder.Language.BuildColumnName(table.Alias, rawName);
        }
    }
}
