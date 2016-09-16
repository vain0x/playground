using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    /// <summary>
    /// SQLの式を表す。
    /// </summary>
    /// <typeparam name="TType"></typeparam>
    public abstract class SqlExpression<TType>
        : SqlPart
        where TType: ISqlTypeTag
    {
        internal SqlBuilder SqlBuilder { get; }

        protected SqlExpression(SqlBuilder sqlBuilder)
        {
            SqlBuilder = sqlBuilder;
        }

        public override string ToString() =>
            string.Join(" ", Tokens);

        public AliasedSqlExpression<TType> As(string alias)
        {
            return new AliasedExpression<TType>(SqlBuilder, this, alias);
        }
    }

    public class CompoundExpression<TType>
        : SqlExpression<TType>
        where TType: ISqlTypeTag
    {
        SqlPart Part { get; }

        #region SqlPart
        internal sealed override IEnumerable<string> Tokens => Part.Tokens;
        internal sealed override IEnumerable<DbParameter> Parameters => Part.Parameters;
        #endregion

        internal CompoundExpression(SqlBuilder sqlBuilder, SqlPart part)
            : base(sqlBuilder)
        {
            Part = part;
        }
    }

    public class AtomicExpression<TType>
        : CompoundExpression<TType>
        where TType: ISqlTypeTag
    {
        internal AtomicExpression(SqlBuilder sqlBuilder, string @string)
            : base(sqlBuilder, SqlPart.FromToken(@string))
        {
        }
    }

    public class ParameterExpression<TValue>
        : SqlExpression<IScalar<TValue>>
    {
        string Name { get; }
        DbParameter Parameter { get; }

        internal ParameterExpression(
            SqlBuilder sqlBuilder,
            string name,
            DbParameter parameter
        )
            : base(sqlBuilder)
        {
            Name = "@" + name;
            Parameter = parameter;
        }

        #region SqlExpression
        internal sealed override IEnumerable<string> Tokens =>
            new[] { Name };

        internal sealed override IEnumerable<DbParameter> Parameters =>
            new[] { Parameter };
        #endregion
    }

    public abstract class AliasedSqlExpression<TType>
        : SqlExpression<TType>
        where TType: ISqlTypeTag
    {
        public abstract string Alias { get; }

        protected AliasedSqlExpression(SqlBuilder sqlBuilder)
            : base(sqlBuilder)
        {
        }
    }

    public class AliasedExpression<TType>
        : AliasedSqlExpression<TType>
        where TType: ISqlTypeTag
    {
        public SqlExpression<TType> Expression { get; }
        public sealed override string Alias { get; }

        public AliasedExpression(
            SqlBuilder sqlBuilder,
            SqlExpression<TType> expression,
            string alias
        )
            : base(sqlBuilder)
        {
            Expression = expression;
            Alias = alias;
        }

        #region SqlPart
        internal override IEnumerable<string> Tokens =>
            SqlBuilder.Language.ConstructAliasedExpression(Expression.Tokens, Alias);

        internal override IEnumerable<DbParameter> Parameters =>
            Expression.Parameters;
        #endregion
    }
}
