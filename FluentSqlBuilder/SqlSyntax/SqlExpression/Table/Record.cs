using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FluentSqlBuilder.Public
{
    public interface IRecord
    {
        object GetValueOrAlternative(string columnName, object alternative);
        void SetValue(string columnName, object value);
    }

    public class DictionaryRecord
        : Dictionary<string, object>
        , IRecord
    {
        #region IRecord
        public object GetValueOrAlternative(string columnName, object alternative)
        {
            object value;
            return TryGetValue(columnName, out value) ? value : alternative;
        }

        public void SetValue(string columnName, object value)
        {
            this[columnName] = value;
        }
        #endregion
    }
}
