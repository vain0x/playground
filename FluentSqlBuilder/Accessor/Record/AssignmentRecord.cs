using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FluentSqlBuilder.SqlSyntax;

namespace FluentSqlBuilder.Accessor
{
    sealed class AssignmentRecord
        : Dictionary<string, ScalarSqlExpression>
        , IExpressionRecord
    {
    }
}
