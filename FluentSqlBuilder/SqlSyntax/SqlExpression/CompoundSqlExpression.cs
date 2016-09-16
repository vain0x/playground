using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FluentSqlBuilder.SqlSyntax
{
    class CompoundSqlExpression<TType>
        : SqlExpression<TType>
        where TType : ISqlTypeTag
    {
        SqlPart Part { get; }

        internal override IEnumerable<SqlToken> Tokens => Part.Tokens;

        internal CompoundSqlExpression(SqlBuilder sqlBuilder, SqlPart part)
            : base(sqlBuilder)
        {
            Part = part;
        }
    }

    sealed class AtomicSqlExpression<TType>
        : CompoundSqlExpression<TType>
        where TType : ISqlTypeTag
    {
        internal AtomicSqlExpression(SqlBuilder sqlBuilder, string @string)
            : base(sqlBuilder, SqlPart.FromString(@string))
        {
        }
    }

    public sealed class ParameterSqlExpression<TValue>
        : SqlExpression<IScalar<TValue>>
    {
        string Name { get; }
        DbParameter Parameter { get; }

        internal ParameterSqlExpression(
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
}
