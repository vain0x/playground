using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FluentSqlBuilder.SqlSyntax
{
    sealed class ConcreteScalarSqlExpression<TValue>
        : ScalarSqlExpression<TValue>
    {
        internal override IEnumerable<SqlToken> Tokens { get; }

        internal ConcreteScalarSqlExpression(SqlBuilder sqlBuilder, IEnumerable<SqlToken> tokens)
            : base(sqlBuilder)
        {
            Tokens = tokens;
        }
    }

    sealed class ConcreteRelationSqlExpression
        : RelationSqlExpression
    {
        internal override IEnumerable<SqlToken> Tokens { get; }

        internal ConcreteRelationSqlExpression(SqlBuilder sqlBuilder, IEnumerable<SqlToken> tokens)
            : base(sqlBuilder)
        {
            Tokens = tokens;
        }
    }

    sealed class ParameterSqlExpression<TValue>
        : ScalarSqlExpression<TValue>
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
