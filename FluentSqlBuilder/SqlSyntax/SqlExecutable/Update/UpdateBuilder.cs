using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FluentSqlBuilder.SqlSyntax
{
    public sealed class UpdateBuilder
    {
        UpdateStatement Statement { get; }

        public UpdateBuilder Where(SqlCondition condition)
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
