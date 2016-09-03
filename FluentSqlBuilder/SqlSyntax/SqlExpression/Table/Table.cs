using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public class Table
        : SqlExpression<IRelation>
        , ITable
    {
        #region ITable
        public string QualifiedName { get; }
        public string RawName { get; }
        public string Alias { get; }

        public IColumn<X> Column<X>(string columnName) =>
            new Column<X>(SqlBuilder, this, columnName);

        #region SqlExpression
        public sealed override IEnumerable<string> Tokens
        {
            get
            {
                yield return QualifiedName;
                if (Alias != RawName)
                {
                    yield return "as";
                    yield return SqlBuilder.Language.BuildIdentifier(null, Alias);
                }
            }
        }

        public sealed override IEnumerable<DbParameter> Parameters =>
            Enumerable.Empty<DbParameter>();
        #endregion
        #endregion

        public Table(SqlBuilder sqlBuilder, string rawName, string alias)
            : base(sqlBuilder)
        {
            Alias = alias;
            RawName = rawName;
            QualifiedName = sqlBuilder.Language.BuildIdentifier(null, rawName);
        }
    }
}
