using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using AsterSql.Core.SqlSyntax;

namespace AsterSql.Core.Accessor
{
    sealed class AssignmentRecord
        : Dictionary<string, ScalarSqlExpression>
        , IExpressionRecord
    {
    }
}
