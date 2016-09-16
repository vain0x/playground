using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
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
