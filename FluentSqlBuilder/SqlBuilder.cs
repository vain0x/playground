using System;
using System.Data;
using FluentSqlBuilder.Detail;
using FluentSqlBuilder.Interface;

namespace FluentSqlBuilder
{
    public class SqlBuilder
    {
        private readonly DbProvider _provider;

        internal DbProvider Provider
        {
            get { return _provider; }
        }

        public SqlBuilder(DbProvider provider)
        {
            _provider = provider;
        }

        #region Expression
        public Expression Table(string qualifier, string tableName)
        {
            throw new NotImplementedException();
        }

        public Expression Table(string tableName)
        {
            if (!_provider.Language.IsTableName(tableName)) throw new ArgumentException("tableName");
            return new Expression(_provider.Language.EscapeTableName(tableName));
        }

        public Expression Column(string qualifier, string tableName)
        {
            throw new NotImplementedException();
        }

        public Expression Column(string columnName)
        {
            if (!_provider.Language.IsColumnName(columnName)) throw new ArgumentException("columnName");
            return new Expression(_provider.Language.EscaleColumnName(columnName));
        }

        public Expression Value(DbType type, object value)
        {
            var name = "p" + Guid.NewGuid().ToString().Replace("-", "");
            var parameter = _provider.ParameterFactory.Create(name, type, value);
            return new ParameterExpression(name, parameter);
        }

        #region Typed value expressions
        public Expression Bool(bool value)
        {
            return Value(DbType.Boolean, value);
        }

        public Expression Int(long value)
        {
            return Value(DbType.Int64, value);
        }

        public Expression String(string value)
        {
            return Value(DbType.String, value);
        }

        public Expression Date(DateTime value)
        {
            return Value(DbType.Date, value);
        }

        public Expression DateTime(DateTime value)
        {
            return Value(DbType.DateTime, value);
        }

        private static readonly Expression _nullExpression =
            new Expression("null");

        public Expression Null
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
            var statement = new SelectStatement(_provider);
            return new FromlessSelectBuilder(statement);
        }
        #endregion
    }
}
