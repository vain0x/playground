using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using AsterSql.SqlSyntax;

namespace AsterSql.TypedRecord
{
    public interface IValueRecord
    {
        object this[string columnName] { get; set; }
    }

    public interface IExpressionRecord
    {
        ScalarSqlExpression this[string columnName] { get; set; }
    }
}
