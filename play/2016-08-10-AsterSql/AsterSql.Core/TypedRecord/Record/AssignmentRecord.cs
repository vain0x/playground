using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using AsterSql.SqlSyntax;

namespace AsterSql.TypedRecord
{
    sealed class AssignmentRecord
        : Dictionary<string, ScalarSqlExpression>
        , IExpressionRecord
    {
    }
}
