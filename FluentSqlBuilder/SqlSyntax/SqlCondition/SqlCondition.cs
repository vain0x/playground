using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public interface ISqlCondition
        : ISqlPart
    {
        SqlBuilder SqlBuilder { get; }

        ConditionBuilder And(ISqlCondition rhs);
        ConditionBuilder Or(ISqlCondition rhs);
    }

    public class SqlCondition
        : ISqlCondition
    {
        public SqlBuilder SqlBuilder { get; }

        ISqlPart Part { get; }

        public SqlCondition(SqlBuilder sqlBuilder, ISqlPart part)
        {
            SqlBuilder = sqlBuilder;
            Part = part;
        }

        #region ISqlPart
        public IEnumerable<string> Tokens => Part.Tokens;
        public IEnumerable<DbParameter> Parameters => Part.Parameters;
        #endregion

        #region ISqlCondition
        public ConditionBuilder And(ISqlCondition rhs) =>
            new ConditionBuilder(SqlBuilder, ConditionCombinator.And)
            .Add(this)
            .Add(rhs);

        public ConditionBuilder Or(ISqlCondition rhs) =>
            new ConditionBuilder(SqlBuilder, ConditionCombinator.Or)
            .Add(this)
            .Add(rhs);
        #endregion
    }
}
