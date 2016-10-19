using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AsterSql.SqlSyntax
{
    public abstract class SqlCondition
        : SqlPart
    {
        internal SqlBuilder SqlBuilder { get; }

        internal SqlCondition(SqlBuilder sqlBuilder)
        {
            SqlBuilder = sqlBuilder;
        }

        public abstract SqlCondition And(SqlCondition rhs);
        public abstract SqlCondition Or(SqlCondition rhs);
    }
}
