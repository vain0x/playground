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

        public override string ToString()
        {
            return string.Join(" ", Tokens);
        }
    }

    public class ConcreteSqlExpression
        : SqlExpression
    {
        public override IEnumerable<string> Tokens { get; }
        public override IEnumerable<DbParameter> Parameters { get; }

        internal ConcreteSqlExpression(IEnumerable<string> strings, IEnumerable<DbParameter> parameters)
        {
            Tokens = strings;
            Parameters = parameters;
        }

        internal ConcreteSqlExpression(IEnumerable<string> strings)
            : this(strings, Enumerable.Empty<DbParameter>())
        {
        }

        internal ConcreteSqlExpression(ISqlPart sqlPart)
            : this(sqlPart.Tokens, sqlPart.Parameters)
        {
        }
    }

    public class CompoundExpression
        : ConcreteSqlExpression
    {
        internal CompoundExpression(IEnumerable<SqlExpression> innerExpressions)
            : base(
                  innerExpressions.SelectMany(x => x.Tokens),
                  innerExpressions.SelectMany(x => x.Parameters)
                  )
        {
        }
    }

    public class AtomicExpression
        : ConcreteSqlExpression
    {
        internal AtomicExpression(string @string)
            : base(new[] { @string })
        {
        }
    }

    public class ParameterExpression
        : ConcreteSqlExpression
    {
        internal ParameterExpression(string name, DbParameter parameter)
            : base(new[] { "@" + name }, new[] { parameter })
        {
        }
    }
}
