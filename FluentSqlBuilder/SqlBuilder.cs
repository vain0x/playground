﻿using System;
using System.Data;
using System.Data.Common;
using System.Linq;
using FluentSqlBuilder.Detail;
using FluentSqlBuilder.Provider;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder
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
        public ITable Table(string qualifier, string tableName)
        {
            throw new NotImplementedException();
        }

        public ITable Table(string tableName)
        {
            return new Table(this, tableName);
        }

        public ISqlExpression<IScalar<X>> Column<X>(string qualifier, string columnName)
        {
            return new AtomicExpression<IScalar<X>>(this, Language.BuildIdentifier(qualifier, columnName));
        }

        public ISqlExpression<IScalar<X>> Column<X>(string columnName)
        {
            return Column<X>(null, columnName);
        }

        ParameterExpression<IScalar<X>> ValueImpl<X>(DbType type, object value)
        {
            var name = "p" + Guid.NewGuid().ToString().Replace("-", "");
            var parameter = Factory.CreateParameter();
            parameter.ParameterName = name;
            parameter.DbType = type;
            parameter.Value = value;
            return new ParameterExpression<IScalar<X>>(this, name, parameter);
        }

        ParameterExpression<IScalar<object>> Value(DbType type, object value) =>
            ValueImpl<object>(type, value);

        #region Typed value expressions
        public ParameterExpression<IScalar<bool>> Bool(bool value)
        {
            return ValueImpl<bool>(DbType.Boolean, value);
        }

        public ParameterExpression<IScalar<long>> Int(long value)
        {
            return ValueImpl<long>(DbType.Int64, value);
        }

        public ParameterExpression<IScalar<string>> String(string value)
        {
            return ValueImpl<string>(DbType.String, value);
        }

        public ParameterExpression<IScalar<DateTime>> Date(DateTime value)
        {
            return ValueImpl<DateTime>(DbType.Date, value);
        }

        public ParameterExpression<IScalar<DateTime>> DateTime(DateTime value)
        {
            return ValueImpl<DateTime>(DbType.DateTime, value);
        }

        public SqlExpression<IScalar<object>> Null
        {
            get { return new AtomicExpression<IScalar<object>>(this, "null"); }
        }
        #endregion
        #endregion

        #region Condition
        public ConditionBuilder And()
        {
            return new ConditionBuilder(this, ConditionCombinator.And);
        }

        public ConditionBuilder Or()
        {
            return new ConditionBuilder(this, ConditionCombinator.Or);
        }
        #endregion

        #region Mainpulation
        public FromlessSelectBuilder Select()
        {
            return new FromlessSelectBuilder(new SelectStatement(this));
        }
        #endregion
    }
}
