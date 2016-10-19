using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using AsterSql.TypedRecord;

namespace AsterSql.Core.SqlSyntax
{
    public sealed class AssignmentlessUpdateBuilder
    {
        UpdateStatement Statement { get; }

        public UpdateBuilder Set(Action<IExpressionRecord> assign)
        {
            assign(Statement.Assignment);
            return new UpdateBuilder(Statement);
        }

        public AssignmentlessUpdateBuilder(UpdateStatement statement)
        {
            Statement = statement;
        }
    }
}
