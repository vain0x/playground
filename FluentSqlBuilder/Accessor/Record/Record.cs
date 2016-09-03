using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Optional;
using FluentSqlBuilder.Detail;

namespace FluentSqlBuilder.Public
{
    public interface IValueRecord
    {
        Option<object> GetValue(string columnName);
        void SetValue(string columnName, object value);
    }

    public interface IExpressionRecord
    {
        ISqlExpression<IScalar> GetValue(string columnName);
        void SetValue(string columnName, ISqlExpression<IScalar> value);
    }
}
