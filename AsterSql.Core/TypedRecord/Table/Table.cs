using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;
using System.Reflection;
using Optional;
using AsterSql.Core;
using AsterSql.SqlSyntax;

namespace AsterSql.TypedRecord
{
    public sealed class Table
        : RelationSqlExpression
        , IAliasedSqlExpression
    {
        Relation Relation { get; }
        Option<string> OptionalAlias { get; }

        public string RawName { get; }

        internal string QuotedName => SqlBuilder.Language.BuildTableName(RawName);
        public string Alias => OptionalAlias.ValueOr(RawName);

        public Column<X> Column<X>(string columnName) =>
            TypedRecord.Column.Create<X>(this, columnName);

        internal override IEnumerable<SqlToken> Tokens
        {
            get
            {
                var tableName = new[] { SqlToken.FromString(QuotedName) };
                return
                    OptionalAlias.Match(
                        alias => SqlBuilder.Language.ConstructAliasedExpression(tableName, alias),
                        () => tableName
                    );
            }
        }

        #region Reflection
        internal IReadOnlyList<IColumn> Columns => Relation.Columns;

        /// <summary>
        /// 修飾されていないクオートされたカラム名のリスト。
        /// "`name`,`age`" など
        /// insert 文に使用するためにキャッシュされる。
        /// </summary>
        internal Lazy<string> ColumnNameList { get; }

        /// <summary>
        /// 各カラムのユニーク名をパラメーター名として使用するときの、パラメーターのリスト。
        /// "@t__c1,@t__c2" など。
        /// insert-values 文に使用するためにキャッシュされる。
        /// </summary>
        internal Lazy<string> ColumnUniqueNameParameterList { get; }
        #endregion

        internal Table(
            SqlBuilder sqlBuilder,
            Relation relation,
            string rawName,
            Option<string> alias
        )
            : base(sqlBuilder)
        {
            Relation = relation;
            OptionalAlias = alias;
            RawName = rawName;

            ColumnNameList =
                Lazy.Create(() =>
                    Columns
                    .Select(c => sqlBuilder.Language.QuoteIdentifier(c.RawName))
                    .Intercalate(',')
                );

            ColumnUniqueNameParameterList =
                Lazy.Create(() =>
                    Columns
                    .Select(c => "@" + c.UniqueName)
                    .Intercalate(',')
                );
        }
    }
}
