using System;
using AsterSql.TypedRecord;

namespace AsterSql.Core.SqlSyntax
{
    public sealed class JoinBuilder<TResult>
    {
        SqlBuilder SqlBuilder { get; }
        JoinType JoinType { get; }
        RelationSqlExpression Relation { get; }
        Func<Join, TResult> Run { get; }

        internal JoinBuilder(
            SqlBuilder sqlBuilder,
            JoinType joinType,
            RelationSqlExpression relation,
            Func<Join, TResult> run
        )
        {
            SqlBuilder = sqlBuilder;
            JoinType = joinType;
            Relation = relation;
            Run = run;
        }

        public TResult On(SqlCondition condition)
        {
            return Run(new JoinOn(JoinType, Relation, condition));
        }

        public TResult Using(IColumn column)
        {
            var columnName = SqlBuilder.Language.QuoteIdentifier(column.RawName);
            return Run(new JoinUsing(JoinType, Relation, columnName));
        }
    }
}
