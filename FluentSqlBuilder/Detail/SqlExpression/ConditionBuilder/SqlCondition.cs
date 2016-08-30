using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FluentSqlBuilder.Detail
{
    public interface ISqlCondition
        : ISqlPart
    {
    }

    public class SqlCondition
        : ISqlCondition
    {
        IEnumerable<ISqlPart> Parts { get; }

        public SqlCondition(IEnumerable<ISqlPart> parts)
        {
            Parts = parts;
        }

        public SqlCondition(params ISqlPart[] parts)
            : this((IEnumerable<ISqlPart>)parts)
        {
        }

        #region ISqlPart
        public IEnumerable<string> Tokens =>
            Parts.SelectMany(part => part.Tokens);

        public IEnumerable<DbParameter> Parameters =>
            Parts.SelectMany(part => part.Parameters);
        #endregion
    }
}
