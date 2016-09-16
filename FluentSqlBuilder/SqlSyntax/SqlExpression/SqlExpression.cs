using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;

namespace FluentSqlBuilder.SqlSyntax
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

    class CompoundExpression<TType>
        : SqlExpression<TType>
        where TType: ISqlTypeTag
    {
        SqlPart Part { get; }

        internal override IEnumerable<SqlToken> Tokens => Part.Tokens;

        internal CompoundExpression(SqlBuilder sqlBuilder, SqlPart part)
            : base(sqlBuilder)
        {
            Part = part;
        }
    }

    sealed class AtomicExpression<TType>
        : CompoundExpression<TType>
        where TType: ISqlTypeTag
    {
        internal AtomicExpression(SqlBuilder sqlBuilder, string @string)
            : base(sqlBuilder, SqlPart.FromString(@string))
        {
        }
    }

    public sealed class ParameterExpression<TValue>
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

        internal override IEnumerable<SqlToken> Tokens =>
            new[] { SqlToken.Create(Name, new[] { Parameter }) };
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

    class AliasedExpression<TType>
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

        internal override IEnumerable<SqlToken> Tokens =>
            Expression.Tokens
            .Concat(
                new[]
                {
                    SqlToken.FromString("as"),
                    SqlToken.FromString(SqlBuilder.Language.QuoteIdentifier(Alias))
                });
    }
}
