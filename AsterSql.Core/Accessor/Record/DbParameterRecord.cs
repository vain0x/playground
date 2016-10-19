using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Optional;

namespace AsterSql.Core.Accessor
{
    sealed class DbParameterRecord
        : Dictionary<string, DbParameter>
        , IValueRecord
    {
        public new object this[string columnName]
        {
            get
            {
                return base[columnName].Value;
            }
            set
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
        }
    }
}
