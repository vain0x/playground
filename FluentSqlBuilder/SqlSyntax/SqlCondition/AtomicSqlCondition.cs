using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public abstract class SqlCondition
        : SqlPart
    {
        internal SqlBuilder SqlBuilder { get; }

        protected SqlCondition(SqlBuilder sqlBuilder)
        {
            SqlBuilder = sqlBuilder;
        }

        public abstract ConditionBuilder And(SqlCondition rhs);
        public abstract ConditionBuilder Or(SqlCondition rhs);
    }

    public sealed class AtomicSqlCondition
        : SqlCondition
    {
        SqlPart Part { get; }

        public AtomicSqlCondition(SqlBuilder sqlBuilder, SqlPart part)
            : base(sqlBuilder)
        {
            Part = part;
        }

        #region SqlPart
        internal override IEnumerable<string> Tokens => Part.Tokens;
        internal override IEnumerable<DbParameter> Parameters => Part.Parameters;
        #endregion

        #region SqlCondition
        public override ConditionBuilder And(SqlCondition rhs) =>
            new ConditionBuilder(SqlBuilder, ConditionCombinator.And)
            .Add(this)
            .Add(rhs);

        public override ConditionBuilder Or(SqlCondition rhs) =>
            new ConditionBuilder(SqlBuilder, ConditionCombinator.Or)
            .Add(this)
            .Add(rhs);
        #endregion
    }
}
