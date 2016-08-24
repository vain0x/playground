using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;

namespace FluentSqlBuilder.Detail
{
    public abstract class SqlExpression
        : ISqlPart
    {
        public abstract IEnumerable<string> Tokens { get; }
        public abstract IEnumerable<DbParameter> Parameters { get; }

        internal SqlBuilder SqlBuilder { get; }

        protected SqlExpression(SqlBuilder sqlBuilder)
        {
            SqlBuilder = sqlBuilder;
        }

        public override string ToString()
        {
            return string.Join(" ", Tokens);
        }

        public AliasedExpression As(string alias)
        {
            if (!SqlBuilder.Language.IsIdentifier(alias))
            {
                throw new ArgumentException(nameof(alias));
            }

            var quotedAlias = SqlBuilder.Language.BuildIdentifier(null, alias);
            return new AliasedExpression(SqlBuilder, this, quotedAlias);
        }
    }

    public class ConcreteSqlExpression
        : SqlExpression
    {
        public sealed override IEnumerable<string> Tokens { get; }
        public sealed override IEnumerable<DbParameter> Parameters { get; }

        internal ConcreteSqlExpression(SqlBuilder sqlBuilder, IEnumerable<string> strings, IEnumerable<DbParameter> parameters)
            : base(sqlBuilder)
        {
            Tokens = strings;
            Parameters = parameters;
        }

        internal ConcreteSqlExpression(SqlBuilder sqlBuilder, IEnumerable<string> strings)
            : this(sqlBuilder, strings, Enumerable.Empty<DbParameter>())
        {
        }

        internal ConcreteSqlExpression(SqlBuilder sqlBuilder, ISqlPart sqlPart)
            : this(sqlBuilder, sqlPart.Tokens, sqlPart.Parameters)
        {
        }
    }

    public class CompoundExpression
        : ConcreteSqlExpression
    {
        internal CompoundExpression(SqlBuilder sqlBuilder, IEnumerable<SqlExpression> innerExpressions)
            : base(
                sqlBuilder,
                innerExpressions.SelectMany(x => x.Tokens),
                innerExpressions.SelectMany(x => x.Parameters)
            )
        {
        }
    }

    public class AtomicExpression
        : ConcreteSqlExpression
    {
        internal AtomicExpression(SqlBuilder sqlBuilder, string @string)
            : base(sqlBuilder, new[] { @string })
        {
        }
    }

    public class ParameterExpression
        : ConcreteSqlExpression
    {
        internal ParameterExpression(SqlBuilder sqlBuilder, string name, DbParameter parameter)
            : base(sqlBuilder, new[] { "@" + name }, new[] { parameter })
        {
        }
    }

    public class AliasedExpression
        : SqlExpression
    {
        public SqlExpression Expression { get; }
        public string Alias { get; }

        public AliasedExpression(SqlBuilder sqlBuilder, SqlExpression expression, string alias)
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
