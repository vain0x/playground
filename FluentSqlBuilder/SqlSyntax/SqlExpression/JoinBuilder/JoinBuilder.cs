using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public class JoinBuilder<TResult>
    {
        SqlBuilder SqlBuilder { get; }
        IRelationalQueryOrCommand Statement { get; }
        JoinType JoinType { get; }
        ISqlExpression<IRelation> Relation { get; }
        TResult Result { get; }

        public JoinBuilder(
            SqlBuilder sqlBuilder,
            IRelationalQueryOrCommand statement,
            JoinType joinType,
            ISqlExpression<IRelation> relation,
            TResult result
        )
        {
            SqlBuilder = sqlBuilder;
            Statement = statement;
            JoinType = joinType;
            Relation = relation;
            Result = result;
        }

        public TResult On(ConditionBuilder condition)
        {
            Statement.Source.Add(new JoinOn(JoinType, Relation, condition));
            return Result;
        }

        public TResult Using(IColumn column)
        {
            var columnName = SqlBuilder.Language.QuoteIdentifier(column.RawName);
            Statement.Source.Add(new JoinUsing(JoinType, Relation, columnName));
            return Result;
        }
    }
}
