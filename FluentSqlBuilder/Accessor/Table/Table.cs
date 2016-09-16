using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;
using System.Reflection;
using Optional;
using FluentSqlBuilder.SqlSyntax;

namespace FluentSqlBuilder.Accessor
{
    public sealed class Table
        : AliasedSqlExpression<IRelation>
    {
        object Relation { get; }
        Option<string> OptionalAlias { get; }

        public string RawName { get; }

        internal string QuotedName => SqlBuilder.Language.BuildTableName(RawName);
        public override string Alias => OptionalAlias.ValueOr(RawName);

        public Column<X> Column<X>(string columnName) =>
            new ConcreteColumn<X>(SqlBuilder, this, columnName);

        internal override IEnumerable<SqlToken> Tokens
        {
            get
            {
                yield return SqlToken.FromString(QuotedName);
                foreach (var alias in OptionalAlias)
                {
                    yield return SqlToken.FromString("as");
                    yield return SqlToken.FromString(SqlBuilder.Language.QuoteIdentifier(alias));
                }
            }
        }

        #region Reflection
        bool IsColumnType(Type type)
        {
            return type.GetInterface(nameof(IColumn)) != null;
        }

        internal IEnumerable<PropertyInfo> ColumnProperties()
        {
            return
                Relation.GetType()
                .GetProperties()
                .Where(propertyInfo => IsColumnType(propertyInfo.PropertyType));
        }

        internal Lazy<IReadOnlyList<IColumn>> Columns { get; }

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
            object relation,
            string rawName,
            Option<string> alias
        )
            : base(sqlBuilder)
        {
            Relation = relation;
            OptionalAlias = alias;
            RawName = rawName;

            Columns =
                Lazy.Create(() =>
                    (IReadOnlyList<IColumn>)
                    ColumnProperties()
                    .Select(p => (IColumn)p.GetValue(Relation))
                    .ToArray()
                );

            ColumnNameList =
                Lazy.Create(() =>
                    Columns.Value
                    .Select(c => sqlBuilder.Language.QuoteIdentifier(c.RawName))
                    .Intercalate(',')
                );

            ColumnUniqueNameParameterList =
                Lazy.Create(() =>
                    Columns.Value
                    .Select(c => "@" + c.UniqueName)
                    .Intercalate(',')
                );
        }
    }
}
