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
        : Dictionary<string, ISqlExpression<IScalar>>
        , IExpressionRecord
    {
        #region IRecord
        public ISqlExpression<IScalar> GetValue(string columnName)
        {
            return this[columnName];
        }

        public void SetValue(string columnName, ISqlExpression<IScalar> value)
        {
            this[columnName] = value;
        }
        #endregion
    }
}
