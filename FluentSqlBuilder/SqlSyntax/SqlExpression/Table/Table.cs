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

        public string QuotedName => SqlBuilder.Language.BuildTableName(RawName);

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
        #endregion

        public Table(SqlBuilder sqlBuilder, object relation, string rawName, Option<string> alias)
            : base(sqlBuilder)
        {
            Relation = relation;
            OptionalAlias = alias;
            RawName = rawName;
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

        IEnumerable<IColumn> Columns =>
            ColumnProperties()
            .Select(p => (IColumn)p.GetValue(Relation))
            .ToArray();
        #endregion

        public DbCommand InsertCommand(Action<IRecord> setter)
        {
            var columns = Columns;
            var parameters =
                columns
                .Select(column => SqlBuilder.CreateParameter(column.UniqueName, column.DbType))
                .ToArray();

            var record = new DbParameterRecord();
            foreach (var parameter in parameters)
            {
                record[parameter.ParameterName] = parameter;
            }

            setter(record);

            var columnNameList =
                columns
                .Select(c => SqlBuilder.Language.QuoteIdentifier(c.RawName))
                .Intercalate(',');
            var parameterList =
                columns
                .Select(c => "@" + c.UniqueName)
                .Intercalate(',');

            var dbCommand = SqlBuilder.Provider.Factory.CreateCommand();
            dbCommand.CommandText =
                $"insert into {QuotedName} ({columnNameList}) values ({parameterList})";
            dbCommand.Parameters.AddRange(parameters);
            return dbCommand;
        }
    }
}
