using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;
using Optional;
using FluentSqlBuilder.Detail;
using FluentSqlBuilder.Provider;

namespace FluentSqlBuilder.Public
{
    public class SqlBuilder
    {
        #region Provider
        internal DbProvider Provider { get; }

        internal SqlLanguage Language => Provider.Language;
        internal DbProviderFactory Factory => Provider.Factory;

        internal DbParameter CreateParameter(string name, DbType dbType)
        {
            var parameter = Factory.CreateParameter();
            parameter.ParameterName = name;
            parameter.DbType = dbType;
            return parameter;
        }

        internal DbCommand CreateCommand(IEnumerable<SqlToken> tokens)
        {
            // Coerce the enumerable.
            var tokenList =
                tokens.ToArray();
            var sql =
                string.Join(" ", tokenList.Select(t => t.String));
            var parameterList =
                tokenList
                .SelectMany(t => t.Parameters)
                .Distinct()
                .ToArray();

            var command = Factory.CreateCommand();
            command.CommandText = sql;
            command.Parameters.AddRange(parameterList);
            return command;
        }
        #endregion

        internal SqlConditionConstant SqlConditionConstant { get; }

        public SqlBuilder(DbProvider provider)
        {
            Provider = provider;

            SqlConditionConstant = new SqlConditionConstant(this);
        }

        #region Expression
        public Table Table(object r, string tableName, Option<string> alias) =>
            new Table(this, r, tableName, alias);

        internal string GenerateUniqueName()
        {
            return "p" + Guid.NewGuid().ToString().Replace("-", "");
        }

        public ParameterExpression<X> Value<X>(DbType type, X value)
        {
            var name = GenerateUniqueName();
            var parameter = Factory.CreateParameter();
            parameter.ParameterName = name;
            parameter.DbType = type;
            parameter.Value = value;
            return new ParameterExpression<X>(this, name, parameter);
        }
        
        #region Typed value expressions
        public ParameterExpression<bool> Bool(bool value) =>
            Value(DbType.Boolean, value);

        public ParameterExpression<int> Int32(int value) =>
            Value(DbType.Int32, value);

        public ParameterExpression<long> Int(long value) =>
            Value(DbType.Int64, value);

        public ParameterExpression<double> Float(double value) =>
            Value(DbType.Double, value);

        public ParameterExpression<string> String(string value) =>
            Value(DbType.String, value);

        public ParameterExpression<DateTime> DateTime(DateTime value) =>
            Value(DbType.DateTime, value);

        public SqlExpression<IScalar<X>> Null<X>() =>
            new AtomicExpression<IScalar<X>>(this, "null");

        public SqlCondition True =>
            SqlConditionConstant.True;

        public SqlCondition False =>
            SqlConditionConstant.False;
        #endregion
        #endregion

        #region Condition
        public ConditionBuilder And() =>
            new ConditionBuilder(this, SqlConditionConstant.And);

        public ConditionBuilder Or() =>
            new ConditionBuilder(this, SqlConditionConstant.Or);
        #endregion

        #region Mainpulation
        public FromlessSelectBuilder Select()
        {
            return new FromlessSelectBuilder(this, Option.None<CombinedSelectStatement>());
        }

        public DbCommand InsertValues(Table table, Action<IValueRecord> setter)
        {
            return InsertBuilder.InsertValuesCommand(this, table, setter);
        }

        public AssignmentlessUpdateBuilder Update(Table table)
        {
            return new AssignmentlessUpdateBuilder(new UpdateStatement(this, table));
        }
        #endregion
    }
}
