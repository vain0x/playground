using System;
using System.Data.Common;
using System.Linq;
using AsterSql.TypedRecord;

namespace AsterSql.Core.SqlSyntax
{
    public sealed class FieldlessSelectBuilder
    {
        SelectStatement Statement { get; }
        
        internal FieldlessSelectBuilder(SelectStatement statement)
        {
            Statement = statement;
        }

        #region Join
        JoinBuilder<FieldlessSelectBuilder> Join(
            RelationSqlExpression relation,
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

        public JoinBuilder<FieldlessSelectBuilder> Join(RelationSqlExpression relation)
        {
            return Join(relation, JoinType.Inner);
        }
        #endregion

        public FieldlessSelectBuilder Where(SqlCondition condition)
        {
            Statement.WhereCondition.Add(condition);
            return this;
        }

        public FieldlessSelectBuilder GroupBy(ScalarSqlExpression expression)
        {
            Statement.GroupKeys.Add(expression);
            return this;
        }

        #region OrderBy
        FieldlessSelectBuilder OrderByImpl(
            ScalarSqlExpression expression,
            OrderDirection direction
        )
        {
            Statement.OrderKeys.Add(new OrderKey(expression, direction));
            return this;
        }

        public FieldlessSelectBuilder OrderBy(ScalarSqlExpression column)
        {
            return OrderByImpl(column, OrderDirection.Ascending);
        }

        public FieldlessSelectBuilder OrderByDescending(ScalarSqlExpression column)
        {
            return OrderByImpl(column, OrderDirection.Descending);
        }
        #endregion

        #region Field
        public SelectBuilder Field(ScalarSqlExpression expression)
        {
            Statement.Fields.Add(expression);
            return new SelectBuilder(Statement);
        }

        public SelectBuilder FieldAll<R>(R relation)
            where R: RelationSqlExpression, IAliasedSqlExpression
        {
            Statement.AddFieldAll(relation);
            return new SelectBuilder(Statement);
        }

        public ScalarSqlExpression<X> ToScalar<X>(ScalarSqlExpression<X> expression)
        {
            Field(expression);
            return Statement.ToScalar<X>();
        }

        ScalarSqlExpression<X> Quantify<X>(
            ScalarSqlExpression<X> expression,
            string quantifier
        )
        {
            Field(expression);
            return Statement.Quantify<X>(quantifier);
        }

        public ScalarSqlExpression<X> Any<X>(ScalarSqlExpression<X> expression)
        {
            return Quantify(expression, "any");
        }

        public ScalarSqlExpression<X> All<X>(ScalarSqlExpression<X> expression)
        {
            return Quantify(expression, "all");
        }
        #endregion

        public SqlCondition Exists()
        {
            Statement.AddFieldAll();
            return
                new AtomicSqlCondition(
                    Statement.SqlBuilder,
                    new[] { SqlToken.FromString("exists") }
                    .Concat(Statement.ToRelation().Tokens)
                );
        }

        public DbCommand Insert(Table table, Action<IExpressionRecord> setter)
        {
            return InsertBuilder.InsertSelectCommand(Statement, table, setter);
        }
    }
}
