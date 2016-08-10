using System.Collections.Generic;
using System.Data.Common;

namespace FluentSqlBuilder.Detail
{
    public class Expression
    {
        string String { get; }
        
        public override string ToString()
        {
            return String;
        }

        IEnumerable<DbParameter> ParametersOrNull { get; }

        public virtual IEnumerable<DbParameter> Parameters
        {
            get
            {
                if (ParametersOrNull == null) yield break;
                foreach (var parameter in ParametersOrNull)
                {
                    yield return parameter;
                }
            }
        }

        internal Expression(string @string, IEnumerable<DbParameter> parametersOrNull)
        {
            String = @string;
            ParametersOrNull = parametersOrNull;
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
