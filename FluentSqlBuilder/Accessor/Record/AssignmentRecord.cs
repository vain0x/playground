using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Optional;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public class AssignmentRecord
        : Dictionary<string, SqlExpression<IScalar>>
        , IExpressionRecord
    {
    }
}
