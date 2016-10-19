using System;
using System.Collections.Generic;
using System.Data;
using System.Data.Common;
using System.Linq;
using Optional;
using FluentSqlBuilder.Accessor;
using FluentSqlBuilder.SqlSyntax;
using FluentSqlBuilder.Provider;

namespace FluentSqlBuilder
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
        public Table Table(Relation r, string tableName, Option<string> alias)
        {
            return new Table(this, r, tableName, alias);
        }

        internal string GenerateUniqueName()
        {
            return "p" + Guid.NewGuid().ToString().Replace("-", "");
        }

        public ScalarSqlExpression<X> Value<X>(DbType type, X value)
        {
            var name = GenerateUniqueName();
            var parameter = Factory.CreateParameter();
            parameter.ParameterName = name;
            parameter.DbType = type;
            parameter.Value = value;
            return new ParameterSqlExpression<X>(this, name, parameter);
        }
        
        #region Typed value expressions
        public ScalarSqlExpression<bool> Bool(bool value)
        {
            return Value(DbType.Boolean, value);
        }

        public ScalarSqlExpression<int> Int32(int value)
        {
            return Value(DbType.Int32, value);
        }

        public ScalarSqlExpression<long> Int(long value)
        {
            return Value(DbType.Int64, value);
        }

        public ScalarSqlExpression<double> Float(double value)
        {
            return Value(DbType.Double, value);
        }

        public ScalarSqlExpression<string> String(string value)
        {
            return Value(DbType.String, value);
        }

        public ScalarSqlExpression<DateTime> DateTime(DateTime value)
        {
            return Value(DbType.DateTime, value);
        }

        public ScalarSqlExpression<X> Null<X>()
        {
            var tokens = new[] { SqlToken.FromString("null") };
            return new ConcreteScalarSqlExpression<X>(this, tokens);
        }

        public SqlCondition True =>
            SqlConditionConstant.True;

        public SqlCondition False =>
            SqlConditionConstant.False;
        #endregion
        #endregion

        #region Condition
        public SqlCondition And()
        {
            return new ConditionBuilder(this, SqlConditionConstant.And);
        }

        public SqlCondition Or()
        {
            return new ConditionBuilder(this, SqlConditionConstant.Or);
        }
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
