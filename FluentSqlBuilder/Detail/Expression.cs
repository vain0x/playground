using System.Collections.Generic;
using System.Data.Common;

namespace FluentSqlBuilder.Detail
{
    public class Expression
    {
        private readonly string _string;
        
        public override string ToString()
        {
            return _string;
        }

        private readonly IEnumerable<DbParameter> _parametersOrNull = null;

        public virtual IEnumerable<DbParameter> Parameters
        {
            get
            {
                if (_parametersOrNull == null) yield break;
                foreach (var parameter in _parametersOrNull)
                {
                    yield return parameter;
                }
            }
        }

        internal Expression(string @string, IEnumerable<DbParameter> parametersOrNull)
        {
            _string = @string;
            _parametersOrNull = parametersOrNull;
        }

        internal Expression(string @string)
            : this(@string, null)
        {
        }
    }
    
    public class ParameterExpression
        : Expression
    {
        internal ParameterExpression(string name, DbParameter parameter)
            : base("@" + name, new[] { parameter })
        {
        }
    }
}
