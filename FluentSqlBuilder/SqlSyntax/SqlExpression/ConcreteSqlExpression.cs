using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FluentSqlBuilder.SqlSyntax
{
    sealed class CompoundSqlExpression<TType>
        : SqlExpression<TType>
        where TType : ISqlTypeTag
    {
        internal override IEnumerable<SqlToken> Tokens { get; }

        internal CompoundSqlExpression(SqlBuilder sqlBuilder, IEnumerable<SqlToken> tokens)
            : base(sqlBuilder)
        {
            Tokens = tokens;
        }
    }

    sealed class AtomicSqlExpression<TType>
        : SqlExpression<TType>
        where TType : ISqlTypeTag
    {
        string String { get; }

        internal override IEnumerable<SqlToken> Tokens =>
            new[] { SqlToken.FromString(String) };

        internal AtomicSqlExpression(SqlBuilder sqlBuilder, string @string)
            : base(sqlBuilder)
        {
            String = @string;
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
