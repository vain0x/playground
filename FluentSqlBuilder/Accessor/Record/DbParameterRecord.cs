using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Optional;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public class DbParameterRecord
        : Dictionary<string, DbParameter>
        , IValueRecord
    {
        #region IRecord
        public Option<object> GetValue(string columnName)
        {
            DbParameter parameter;
            return TryGetValue(columnName, out parameter)
                ? Option.Some<object>(parameter)
                : Option.None<object>();
        }

        public void SetValue(string columnName, object value)
        {
            DbParameter parameter;
            if (TryGetValue(columnName, out parameter))
            {
                parameter.Value = value;
            }
            else
            {
                throw new KeyNotFoundException(nameof(columnName));
            }
        }
        #endregion
    }
}
