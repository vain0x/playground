using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FluentSqlBuilder.SqlSyntax
{
    sealed class AtomicSqlCondition
        : SqlCondition
    {
        SqlPart Part { get; }

        public AtomicSqlCondition(SqlBuilder sqlBuilder, SqlPart part)
            : base(sqlBuilder)
        {
            Part = part;
        }

        internal override IEnumerable<SqlToken> Tokens => Part.Tokens;

        #region SqlCondition
        public override SqlCondition And(SqlCondition rhs) =>
            SqlBuilder.And().And(this).And(rhs);

        public override SqlCondition Or(SqlCondition rhs) =>
            SqlBuilder.Or().Or(this).Or(rhs);
        #endregion
    }
}
