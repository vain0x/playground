using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;
using System.Reflection;
using Optional;
using FluentSqlBuilder.Detail;

namespace FluentSqlBuilder.Public
{
    public class Table
        : SqlExpression<IRelation>
        , IAliasedSqlExpression<IRelation>
    {
        object Relation { get; }
        Option<string> OptionalAlias { get; }

        public string RawName { get; }

        internal string QuotedName => SqlBuilder.Language.BuildTableName(RawName);
        public string Alias => OptionalAlias.ValueOr(RawName);

        public IColumn<X> Column<X>(string columnName) =>
            new Column<X>(SqlBuilder, this, columnName);

        #region SqlExpression
        public sealed override IEnumerable<string> Tokens
        {
            get
            {
                var tableName = new[] { QuotedName };
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

        internal IEnumerable<IColumn> Columns =>
            ColumnProperties()
            .Select(p => (IColumn)p.GetValue(Relation))
            .ToArray();

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
