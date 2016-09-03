using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FluentSqlBuilder.Detail;

namespace FluentSqlBuilder.Public
{
    public interface IValueRecord
    {
        object this[string columnName] { get; set; }
    }

    public interface IExpressionRecord
    {
        ISqlExpression<IScalar> this[string columnName] { get; set; }
    }
}
