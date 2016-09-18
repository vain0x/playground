using System.Collections.Generic;
using System.Data;
using FluentSqlBuilder.SqlSyntax;

namespace FluentSqlBuilder.Accessor
{
    public interface IColumn
    {
        string QualifiedName { get; }
        string UniqueName { get; }
        string RawName { get; }
        DbType DbType { get; }
    }

    public sealed class Column<TValue>
        : ScalarSqlExpression<TValue>
        , IColumn
    {
        public string RawName { get; }
        public string UniqueName { get; }
        public string QualifiedName { get; }

        internal override IEnumerable<SqlToken> Tokens { get; }

        internal Column(
            SqlBuilder sqlBuilder,
            string rawName,
            string uniqueName,
            string qualifiedName,
            IEnumerable<SqlToken> tokens
        )
            : base(sqlBuilder)
        {
            RawName = rawName;
            UniqueName = uniqueName;
            QualifiedName = qualifiedName;
            Tokens = tokens;
        }

        public DbType DbType
        {
            get
            {
                return SqlBuilder.Provider.DbTypeMap.DbTypeFromType(typeof(TValue));
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

        public ScalarSqlExpression<TValue> this[IExpressionRecord record]
        {
            get
            {
                return record[UniqueName].Unbox<TValue>();
            }
            set
            {
                record[UniqueName] = value;
            }
        }
    }

    public static class Column
    {
        /// <summary>
        /// Creates a column of a table.
        /// </summary>
        /// <typeparam name="X"></typeparam>
        /// <param name="table"></param>
        /// <param name="columnName"></param>
        /// <returns></returns>
        internal static Column<X> Create<X>(Table table, string columnName)
        {
            var sqlBuilder = table.SqlBuilder;
            var uniqueName = sqlBuilder.Language.ConcatIdentifiers(table.Alias, columnName);
            var qualifiedName = sqlBuilder.Language.BuildColumnName(table.Alias, columnName);
            var tokens = new[] { SqlToken.FromString(qualifiedName) };
            return new Column<X>(sqlBuilder, columnName, uniqueName, qualifiedName, tokens);
        }
    }
}
