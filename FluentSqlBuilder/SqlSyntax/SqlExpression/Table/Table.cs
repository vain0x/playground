﻿using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using Optional;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public class Table
        : SqlExpression<IRelation>
        , ITable
    {
        public Option<string> OptionalAlias { get; }

        #region ITable
        public string RawName { get; }
        public string Alias => OptionalAlias.ValueOr(RawName);

        public IColumn<X> Column<X>(string columnName) =>
            new Column<X>(SqlBuilder, this, columnName);

        #region SqlExpression
        public sealed override IEnumerable<string> Tokens
        {
            get
            {
                var tableName = new[] { SqlBuilder.Language.BuildTableName(RawName) };
                return
                    OptionalAlias.Match(
                        alias =>
                            SqlBuilder.Language
                            .ConstructAliasedExpression(tableName, alias),
                        () => tableName
                    );
            }
        }

        public sealed override IEnumerable<DbParameter> Parameters =>
            Enumerable.Empty<DbParameter>();
        #endregion
        #endregion

        public Table(SqlBuilder sqlBuilder, string rawName, Option<string> alias)
            : base(sqlBuilder)
        {
            OptionalAlias = alias;
            RawName = rawName;
        }
    }
}
