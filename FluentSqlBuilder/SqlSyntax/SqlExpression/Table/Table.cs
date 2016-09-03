using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;
using System.Reflection;
using Optional;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public class Table
        : SqlExpression<IRelation>
        , ITable
    {
        object Relation { get; }
        public Option<string> OptionalAlias { get; }

        #region ITable
        public string RawName { get; }

        public string QuotedName => SqlBuilder.Language.BuildTableName(RawName);
        public string Alias => OptionalAlias.ValueOr(RawName);

        public IColumn<X> Column<X>(string columnName) =>
            new Column<X>(SqlBuilder, this, columnName);
        #endregion

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

        public IEnumerable<IColumn> Columns =>
            ColumnProperties()
            .Select(p => (IColumn)p.GetValue(Relation))
            .ToArray();

        /// <summary>
        /// 修飾されていないクオートされたカラム名のリスト。
        /// "`name`,`age`" など
        /// insert 文に使用するためにキャッシュされる。
        /// </summary>
        public Lazy<string> ColumnNameList { get; }

        /// <summary>
        /// 各カラムのユニーク名をパラメーター名として使用するときの、パラメーターのリスト。
        /// "@t__c1,@t__c2" など。
        /// insert-values 文に使用するためにキャッシュされる。
        /// </summary>
        public Lazy<string> ColumnUniqueNameParameterList { get; }
        #endregion

        public Table(SqlBuilder sqlBuilder, object relation, string rawName, Option<string> alias)
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
