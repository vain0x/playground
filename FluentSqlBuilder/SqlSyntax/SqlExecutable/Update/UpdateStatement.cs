using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public sealed class UpdateStatement
        : SqlPart
        , ISqlExecutable
    {
        public SqlBuilder SqlBuilder { get; }
        public Table Table { get; }
        public AssignmentRecord Assignment { get; }
        public ConditionBuilder WhereCondition { get; }

        public IEnumerable<KeyValuePair<IColumn, SqlExpression<IScalar>>> AssignmentList()
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

        #region Tokens
        IEnumerable<SqlToken> AssignmentTokens =>
            AssignmentList()
            .Select(kv =>
                new[]
                {
                    SqlToken.FromString(kv.Key.QualifiedName),
                    SqlToken.FromString("=")
                }
                .Concat(kv.Value.Tokens)
            )
            .Intercalate(new[] { SqlToken.FromString(",") });

        IEnumerable<SqlToken> SetTokens =>
            new[] { SqlToken.FromString("set") }.Concat(AssignmentTokens);

        IEnumerable<SqlToken> WhereTokens =>
            WhereCondition.IsTrivial
            ? Enumerable.Empty<SqlToken>()
            :
                new[] { SqlToken.FromString("where") }
                .Concat(WhereCondition.Tokens);

        internal override IEnumerable<SqlToken> Tokens =>
            new[] { SqlToken.FromString("update") }
            .Concat(Table.Tokens)
            .Concat(SetTokens)
            .Concat(WhereTokens);
        #endregion

        #region ISqlExecutable
        public DbCommand ToCommand()
        {
            return SqlBuilder.CreateCommand(Tokens);
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
