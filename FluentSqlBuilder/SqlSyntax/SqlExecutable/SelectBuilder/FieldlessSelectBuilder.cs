using System;
using System.Data.Common;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public class FieldlessSelectBuilder
    {
        SelectStatement Statement { get; }
        
        public FieldlessSelectBuilder(SelectStatement statement)
        {
            Statement = statement;
        }

        #region From
        public FieldlessSelectBuilder From(ISqlExpression<IRelation> relation)
        {
            Statement.Source.Add(relation);
            return this;
        }
        #endregion

        #region Join
        JoinBuilder<FieldlessSelectBuilder> Join(
            ISqlExpression<IRelation> relation,
            JoinType joinType
        )
        {
            return
                new JoinBuilder<FieldlessSelectBuilder>(
                    Statement.SqlBuilder,
                    Statement,
                    joinType,
                    relation,
                    this
                );
        }

        public JoinBuilder<FieldlessSelectBuilder> Join(ISqlExpression<IRelation> relation) =>
            Join(relation, JoinType.Inner);
        #endregion

        #region Where
        public FieldlessSelectBuilder Where(ISqlCondition condition)
        {
            Statement.WhereCondition.Add(condition);
            return this;
        }
        #endregion

        #region GroupBy
        public FieldlessSelectBuilder GroupBy<X>(ISqlExpression<IScalar<X>> expression)
        {
            Statement.GroupKeys.Add(expression);
            return this;
        }
        #endregion

        #region OrderBy
        FieldlessSelectBuilder OrderByImpl<X>(
            ISqlExpression<IScalar<X>> expression,
            OrderDirection direction
        )
        {
            Statement.OrderKeys.Add(new OrderKey(expression, direction));
            return this;
        }

        public FieldlessSelectBuilder OrderBy<X>(ISqlExpression<IScalar<X>> column) =>
            OrderByImpl(column, OrderDirection.Ascending);

        public FieldlessSelectBuilder OrderByDescending<X>(ISqlExpression<IScalar<X>> column) =>
            OrderByImpl(column, OrderDirection.Descending);
        #endregion

        #region Field
        public SelectBuilder Field<X>(ISqlExpression<IScalar<X>> expression)
        {
            Statement.Fields.Add(expression);
            return new SelectBuilder(Statement);
        }

        public SelectBuilder FieldAll(IAliasedSqlExpression<IRelation> relation)
        {
            Statement.AddFieldAll(relation);
            return new SelectBuilder(Statement);
        }

        public ISqlExpression<IScalar<X>> ToScalar<X>(ISqlExpression<IScalar<X>> expression)
        {
            Field(expression);
            return Statement.ToScalar<X>();
        }

        public ISqlExpression<IRelation<X>> ToSequence<X>(ISqlExpression<IScalar<X>> expression)
        {
            Field(expression);
            return Statement.ToSequence<X>();
        }
        #endregion

        #region Exists
        public ISqlCondition Exists()
        {
            Statement.AddFieldAll();
            return
                new SqlCondition(
                    Statement.SqlBuilder,
                    SqlPart.FromToken("exists").Concat(Statement.ToRelation())
                );
        }
        #endregion

        #region Insert
        public DbCommand Insert(Table table, Action<IExpressionRecord> setter)
        {
            return InsertBuilder.InsertSelectCommand(Statement, table, setter);
        }
        #endregion
    }
}
