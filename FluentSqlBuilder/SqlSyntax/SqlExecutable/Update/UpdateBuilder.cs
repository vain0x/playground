using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public class UpdateBuilder
    {
        UpdateStatement Statement { get; }

        public UpdateBuilder Where(ISqlCondition condition)
        {
            Statement.WhereCondition.Add(condition);
            return this;
        }

        public DbCommand ToCommand()
        {
            return Statement.ToCommand();
        }

        public UpdateBuilder(UpdateStatement statement)
        {
            Statement = statement;
        }
    }
}
