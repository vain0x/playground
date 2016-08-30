using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    /// <summary>
    /// Represents a SQL expression.
    /// TODO: Add "where TType: ISqlTypeTag".
    /// </summary>
    /// <typeparam name="TType"></typeparam>
    public interface ISqlExpression<out TType>
        : ISqlPart
    {
        SqlBuilder SqlBuilder { get; }

        IAliasedSqlExpression<TType> As(string alias);
    }

    public abstract class SqlExpression<TType>
        : ISqlExpression<TType>
    {
        #region ISqlPart
        public abstract IEnumerable<string> Tokens { get; }
        public abstract IEnumerable<DbParameter> Parameters { get; }
        #endregion

        protected SqlExpression(SqlBuilder sqlBuilder)
        {
            SqlBuilder = sqlBuilder;
        }

        public override string ToString() =>
            string.Join(" ", Tokens);

        #region ISqlExpression
        public SqlBuilder SqlBuilder { get; }

        public IAliasedSqlExpression<TType> As(string alias)
        {
            if (!SqlBuilder.Language.IsIdentifier(alias))
            {
                throw new ArgumentException(nameof(alias));
            }

            var quotedAlias = SqlBuilder.Language.BuildIdentifier(null, alias);
            return new AliasedExpression<TType>(SqlBuilder, this, quotedAlias);
        }
        #endregion
    }

    public class CompoundExpression<TType>
        : SqlExpression<TType>
    {
        ISqlPart Part { get; }

        #region SqlExpression
        public sealed override IEnumerable<string> Tokens => Part.Tokens;
        public sealed override IEnumerable<DbParameter> Parameters => Part.Parameters;
        #endregion

        internal CompoundExpression(SqlBuilder sqlBuilder, ISqlPart part)
            : base(sqlBuilder)
        {
            Part = part;
        }
    }

    public class AtomicExpression<TType>
        : CompoundExpression<TType>
    {
        internal AtomicExpression(SqlBuilder sqlBuilder, string @string)
            : base(sqlBuilder, SqlPart.FromToken(@string))
        {
        }
    }

    public class ParameterExpression<TType>
        : SqlExpression<TType>
    {
        string Name { get; }
        DbParameter Parameter { get; }

        internal ParameterExpression(SqlBuilder sqlBuilder, string name, DbParameter parameter)
            : base(sqlBuilder)
        {
            Name = "@" + name;
            Parameter = parameter;
        }

        #region SqlExpression
        public sealed override IEnumerable<string> Tokens =>
            new[] { Name };

        public sealed override IEnumerable<DbParameter> Parameters =>
            new[] { Parameter };
        #endregion
    }

    public interface IAliasedSqlExpression<out TType>
        : ISqlPart
    {
    }

    public class AliasedExpression<TType>
        : SqlExpression<IScalar<object>>
        , IAliasedSqlExpression<TType>
    {
        public ISqlExpression<TType> Expression { get; }
        public string Alias { get; }

        public AliasedExpression(SqlBuilder sqlBuilder, ISqlExpression<TType> expression, string alias)
            : base(sqlBuilder)
        {
            Expression = expression;
            Alias = alias;
        }

        #region ISqlPart
        public override IEnumerable<string> Tokens
        {
            get
            {
                foreach (var token in Expression.Tokens) yield return token;
                yield return "as";
                yield return Alias;
            }
        }

        public override IEnumerable<DbParameter> Parameters =>
            Expression.Parameters;
        #endregion
    }
}
