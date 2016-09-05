using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public class UpdateStatement
        : ISqlExecutable
        , ISqlPart
    {
        public SqlBuilder SqlBuilder { get; }
        public Table Table { get; }
        public AssignmentRecord Assignment { get; }
        public ConditionBuilder WhereCondition { get; }

        public IEnumerable<KeyValuePair<IColumn, ISqlExpression<IScalar>>> AssignmentList()
        {
            return
                Table.Columns.Value
                .Choose(column =>
                {
                    return
                        Assignment.GetValueOrNone(column.UniqueName)
                        .Map(x => KeyValuePair.Create(column, x));
                });
        }

        #region ISqlPart
        public IEnumerable<string> Tokens
        {
            get
            {
                yield return "update";
                foreach (var token in Table.Tokens) yield return token;
                yield return "set";
                var assignmentTokens =
                    AssignmentList()
                    .Select(kv => new[] { kv.Key.QualifiedName, "=" }.Concat(kv.Value.Tokens))
                    .Intercalate(new[] { "," });
                foreach (var token in assignmentTokens) yield return token;
                if (!WhereCondition.IsTrivial)
                {
                    yield return "where";
                    foreach (var token in WhereCondition.Tokens) yield return token;
                }
            }
        }

        public IEnumerable<DbParameter> Parameters
        {
            get
            {
                return
                    Table.Parameters
                    .Concat(AssignmentList().SelectMany(kv => kv.Value.Parameters))
                    .Concat(WhereCondition.Parameters);
            }
        }
        #endregion

        #region ISqlExecutable
        public DbCommand ToCommand()
        {
            return SqlBuilder.CreateCommand(Tokens.Intercalate(' '), Parameters);
        }
        #endregion

        public UpdateStatement(SqlBuilder sqlBuilder, Table table)
        {
            SqlBuilder = sqlBuilder;
            Table = table;
            Assignment = new AssignmentRecord();
            WhereCondition = new ConditionBuilder(sqlBuilder);
        }
    }
}
