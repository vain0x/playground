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

        internal DbCommand CreateCommand(ISqlExecutable expression)
        {
            var command = Factory.CreateCommand();
            command.CommandText = expression.ToString();
            command.Parameters.AddRange(expression.Parameters.ToArray());
            return command;
        }
        #endregion

        public SqlBuilder(DbProvider provider)
        {
            Provider = provider;
        }

        #region Expression
        public ITable Table<R>(R r, string tableName, Option<string> alias) =>
            new Table<R>(this, r, tableName, alias);

        public ParameterExpression<X> Value<X>(DbType type, X value)
        {
            var name = "p" + Guid.NewGuid().ToString().Replace("-", "");
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

        public ISqlExpression<IScalar<object>> Null =>
            new AtomicExpression<IScalar<object>>(this, "null");

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
        #endregion
    }
}
