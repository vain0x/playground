using System;
using System.Data.Common;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public sealed class FieldlessSelectBuilder
    {
        SelectStatement Statement { get; }
        
        public FieldlessSelectBuilder(SelectStatement statement)
        {
            Statement = statement;
        }

        #region Join
        JoinBuilder<FieldlessSelectBuilder> Join(
            SqlExpression<IRelation> relation,
            JoinType joinType
        )
        {
            return
                new JoinBuilder<FieldlessSelectBuilder>(
                    Statement.SqlBuilder,
                    joinType,
                    relation,
                    join =>
                    {
                        Statement.Join(join);
                        return this;
                    }
                );
        }

        public JoinBuilder<FieldlessSelectBuilder> Join(SqlExpression<IRelation> relation) =>
            Join(relation, JoinType.Inner);
        #endregion

        #region Where
        public FieldlessSelectBuilder Where(SqlCondition condition)
        {
            Statement.WhereCondition.Add(condition);
            return this;
        }
        #endregion

        #region GroupBy
        public FieldlessSelectBuilder GroupBy<X>(SqlExpression<IScalar<X>> expression)
        {
            Statement.GroupKeys.Add(expression.Box());
            return this;
        }
        #endregion

        #region OrderBy
        FieldlessSelectBuilder OrderByImpl<X>(
            SqlExpression<IScalar<X>> expression,
            OrderDirection direction
        )
        {
            Statement.OrderKeys.Add(new OrderKey(expression.Box(), direction));
            return this;
        }

        public FieldlessSelectBuilder OrderBy<X>(SqlExpression<IScalar<X>> column) =>
            OrderByImpl(column, OrderDirection.Ascending);

        public FieldlessSelectBuilder OrderByDescending<X>(SqlExpression<IScalar<X>> column) =>
            OrderByImpl(column, OrderDirection.Descending);
        #endregion

        #region Field
        public SelectBuilder Field<X>(SqlExpression<IScalar<X>> expression)
        {
            Statement.Fields.Add(expression);
            return new SelectBuilder(Statement);
        }

        public SelectBuilder FieldAll(AliasedSqlExpression<IRelation> relation)
        {
            Statement.AddFieldAll(relation);
            return new SelectBuilder(Statement);
        }

        public SqlExpression<IScalar<X>> ToScalar<X>(SqlExpression<IScalar<X>> expression)
        {
            Field(expression);
            return Statement.ToScalar<X>();
        }

        SqlExpression<IScalar<X>> Quantify<X>(
            SqlExpression<IScalar<X>> expression,
            string quantifier
        )
        {
            Field(expression);
            return Statement.Quantify<X>(quantifier);
        }

        public SqlExpression<IScalar<X>> Any<X>(SqlExpression<IScalar<X>> expression)
        {
            return Quantify(expression, "any");
        }

        public SqlExpression<IScalar<X>> All<X>(SqlExpression<IScalar<X>> expression)
        {
            return Quantify(expression, "all");
        }
        #endregion

        #region Exists
        public SqlCondition Exists()
        {
            Statement.AddFieldAll();
            return
                new AtomicSqlCondition(
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
