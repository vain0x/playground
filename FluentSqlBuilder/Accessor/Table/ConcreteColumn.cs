using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;
using FluentSqlBuilder.SqlSyntax;

namespace FluentSqlBuilder.Accessor
{
    sealed class ConcreteColumn<TValue>
        : Column<TValue>
    {
        #region IColumn
        public override string QualifiedName { get; }
        public override string UniqueName { get; }
        public override string RawName { get; }

        internal override IEnumerable<SqlToken> Tokens =>
            new[] { SqlToken.FromString(QualifiedName) };
        #endregion

        public ConcreteColumn(SqlBuilder sqlBuilder, Table table, string rawName)
            : base(sqlBuilder)
        {
            RawName = rawName;
            UniqueName = SqlBuilder.Language.ConcatIdentifiers(table.Alias, RawName);
            QualifiedName = sqlBuilder.Language.BuildColumnName(table.Alias, rawName);
        }
    }
}
