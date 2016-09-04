using System;
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

        internal DbCommand CreateCommand(string sql, DbParameter[] parameters)
        {
            var command = Factory.CreateCommand();
            command.CommandText = sql;
            command.Parameters.AddRange(parameters);
            return command;
        }
        #endregion

        public SqlBuilder(DbProvider provider)
        {
            Provider = provider;
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

        public ISqlExpression<IScalar<X>> Null<X>() =>
            new AtomicExpression<IScalar<X>>(this, "null");

        public ConditionBuilder True =>
            new ConditionBuilder(this, ConditionCombinator.And);

        public ConditionBuilder False =>
            new ConditionBuilder(this, ConditionCombinator.Or);
        #endregion
        #endregion

        #region Condition
        public ConditionBuilder And() => True;
        public ConditionBuilder Or() => False;
        #endregion

        #region Mainpulation
        public FromlessSelectBuilder Select()
        {
            return new FromlessSelectBuilder(new SelectStatement(this));
        }

        public DbCommand InsertValues(Table table, Action<IValueRecord> setter)
        {
            return InsertBuilder.InsertValuesCommand(this, table, setter);
        }
        #endregion
    }
}
