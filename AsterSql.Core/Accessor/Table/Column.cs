using System.Collections.Generic;
using System.Data;
using AsterSql.Core.SqlSyntax;

namespace AsterSql.Core.Accessor
{
    public interface IColumn
    {
        string QualifiedName { get; }
        string UniqueName { get; }
        string RawName { get; }
        DbType DbType { get; }

        ScalarSqlExpression AsExpression { get; }
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

        ScalarSqlExpression IColumn.AsExpression => this;

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

        /// <summary>
        /// Creates a virtual column from an expression.
        /// (SQLéÆÇ©ÇÁâºëzÉJÉâÉÄÇê∂ê¨Ç∑ÇÈÅB)
        /// </summary>
        /// <typeparam name="X"></typeparam>
        /// <param name="expression"></param>
        /// <param name="alias"></param>
        /// <returns></returns>
        public static Column<X> ToColumn<X>(this ScalarSqlExpression<X> expression, string alias)
        {
            var sqlBuilder = expression.SqlBuilder;
            var qualifiedName = sqlBuilder.Language.QuoteIdentifier(alias);
            var tokens = expression.As(alias).Tokens;
            return new Column<X>(sqlBuilder, alias, alias, qualifiedName, tokens);
        }
    }
}
