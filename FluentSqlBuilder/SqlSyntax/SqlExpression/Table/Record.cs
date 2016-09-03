using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Optional;

namespace FluentSqlBuilder.Public
{
    public interface IRecord
    {
        Option<object> GetValue(string columnName);
        void SetValue(string columnName, object value);
    }

    public class DictionaryRecord
        : Dictionary<string, object>
        , IRecord
    {
        #region IRecord
        public Option<object> GetValue(string columnName)
        {
            object value;
            return TryGetValue(columnName, out value) ? value.Some() : value.None();
        }

        public void SetValue(string columnName, object value)
        {
            this[columnName] = value;
        }
        #endregion
    }
}
