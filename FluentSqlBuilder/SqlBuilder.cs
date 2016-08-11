using System;
using System.Data;
using FluentSqlBuilder.Detail;
using FluentSqlBuilder.Provider;

namespace FluentSqlBuilder
{
    public class SqlBuilder
    {
        internal DbProvider Provider { get; }

        public SqlBuilder(DbProvider provider)
        {
            Provider = provider;
        }

        #region Expression
        public SqlExpression Table(string qualifier, string tableName)
        {
            throw new NotImplementedException();
        }

        public SqlExpression Table(string tableName)
        {
            if (!Provider.Language.IsTableName(tableName))
            {
                throw new ArgumentException(nameof(tableName));
            }    

            return new AtomicExpression(Provider.Language.EscapeTableName(tableName));
        }

        public SqlExpression Column(string qualifier, string tableName)
        {
            throw new NotImplementedException();
        }

        public SqlExpression Column(string columnName)
        {
            if (!Provider.Language.IsColumnName(columnName))
            {
                throw new ArgumentException(nameof(columnName));
            }

            return new AtomicExpression(Provider.Language.EscaleColumnName(columnName));
        }

        public SqlExpression Value(DbType type, object value)
        {
            var name = "p" + Guid.NewGuid().ToString().Replace("-", "");
            var parameter = Provider.Factory.CreateParameter();
            parameter.ParameterName = name;
            parameter.DbType = type;
            parameter.Value = value;
            return new ParameterExpression(name, parameter);
        }

        #region Typed value expressions
        public SqlExpression Bool(bool value)
        {
            return Value(DbType.Boolean, value);
        }

        public SqlExpression Int(long value)
        {
            return Value(DbType.Int64, value);
        }

        public SqlExpression String(string value)
        {
            return Value(DbType.String, value);
        }

        public SqlExpression Date(DateTime value)
        {
            return Value(DbType.Date, value);
        }

        public SqlExpression DateTime(DateTime value)
        {
            return Value(DbType.DateTime, value);
        }

        static readonly SqlExpression _nullExpression =
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
