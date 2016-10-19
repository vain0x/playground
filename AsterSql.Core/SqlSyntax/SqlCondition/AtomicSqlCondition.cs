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
        internal override IEnumerable<SqlToken> Tokens { get; }

        public AtomicSqlCondition(SqlBuilder sqlBuilder, IEnumerable<SqlToken> tokens)
            : base(sqlBuilder)
        {
            Tokens = tokens;
        }

        #region SqlCondition
        public override SqlCondition And(SqlCondition rhs)
        {
            return SqlBuilder.And().And(this).And(rhs);
        }

        public override SqlCondition Or(SqlCondition rhs)
        {
            return SqlBuilder.Or().Or(this).Or(rhs);
        }
        #endregion
    }
}
