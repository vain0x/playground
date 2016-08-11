using System;
using System.Data;
using System.Data.Common;
using System.Linq;
using FluentSqlBuilder.Detail;
using FluentSqlBuilder.Provider;

namespace FluentSqlBuilder
{
    public class SqlBuilder
    {
        #region Provider
        internal DbProvider Provider { get; }

        internal SqlLanguage Language => Provider.Language;
        internal DbProviderFactory Factory => Provider.Factory;

        internal DbCommand CreateCommand(SqlExpression expression)
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
        public SqlExpression Table(string qualifier, string tableName)
        {
            return new AtomicExpression(Language.BuildIdentifier(qualifier, tableName));
        }

        public SqlExpression Table(string tableName)
        {
            return Table(null, tableName);
        }

        public SqlExpression Column(string qualifier, string columnName)
        {
            return new AtomicExpression(Language.BuildIdentifier(qualifier, columnName));
        }

        public SqlExpression Column(string columnName)
        {
            return Column(null, columnName);
        }

        public ParameterExpression Value(DbType type, object value)
        {
            var name = "p" + Guid.NewGuid().ToString().Replace("-", "");
            var parameter = Factory.CreateParameter();
            parameter.ParameterName = name;
            parameter.DbType = type;
            parameter.Value = value;
            return new ParameterExpression(name, parameter);
        }

        #region Typed value expressions
        public ParameterExpression Bool(bool value)
        {
            return Value(DbType.Boolean, value);
        }

        public ParameterExpression Int(long value)
        {
            return Value(DbType.Int64, value);
        }

        public ParameterExpression String(string value)
        {
            return Value(DbType.String, value);
        }

        public ParameterExpression Date(DateTime value)
        {
            return Value(DbType.Date, value);
        }

        public ParameterExpression DateTime(DateTime value)
        {
            return Value(DbType.DateTime, value);
        }

        static readonly AtomicExpression _nullExpression =
            new AtomicExpression("null");

        public SqlExpression Null
        {
            get { return _nullExpression; }
        }
        #endregion
        #endregion

        #region Condition
        public ConditionBuilder And()
        {
            return new ConditionBuilder(ConditionCombinator.And);
        }

        public ConditionBuilder Or()
        {
            return new ConditionBuilder(ConditionCombinator.Or);
        }
        #endregion

        #region Mainpulation
        public FromlessSelectBuilder Select()
        {
            return new FromlessSelectBuilder(this, new SelectStatement());
        }
        #endregion
    }
}
