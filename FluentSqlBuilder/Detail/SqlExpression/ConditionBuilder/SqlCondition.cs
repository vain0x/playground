using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public interface ISqlCondition
        : ISqlPart
    {
        SqlBuilder SqlBuilder { get; }

        ConditionBuilder And(ISqlCondition rhs);
        ConditionBuilder Or(ISqlCondition rhs);
    }

    public class SqlCondition
        : ISqlCondition
    {
        public SqlBuilder SqlBuilder { get; }

        IEnumerable<ISqlPart> Parts { get; }

        public SqlCondition(SqlBuilder sqlBuilder, IEnumerable<ISqlPart> parts)
        {
            SqlBuilder = sqlBuilder;
            Parts = parts;
        }

        public SqlCondition(SqlBuilder sqlBuilder, params ISqlPart[] parts)
            : this(sqlBuilder, (IEnumerable<ISqlPart>)parts)
        {
        }

        #region ISqlPart
        public IEnumerable<string> Tokens =>
            Parts.SelectMany(part => part.Tokens);

        public IEnumerable<DbParameter> Parameters =>
            Parts.SelectMany(part => part.Parameters);
        #endregion

        #region ISqlCondition
        public ConditionBuilder And(ISqlCondition rhs) =>
            new ConditionBuilder(SqlBuilder, ConditionCombinator.And)
            .Add(this)
            .Add(rhs);

        public ConditionBuilder Or(ISqlCondition rhs) =>
            new ConditionBuilder(SqlBuilder, ConditionCombinator.Or)
            .Add(this)
            .Add(rhs);
        #endregion
    }
}
