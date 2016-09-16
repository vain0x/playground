using System;
using System.Collections.Generic;
using System.Linq;
using FluentSqlBuilder.Detail;

namespace FluentSqlBuilder.Public
{
    public static class SqlExpressionExtensions
    {
        #region Internal
        internal static SqlExpression<T>
            Invoke<T>(
                SqlBuilder sqlBuilder,
                string functionName,
                IEnumerable<SqlExpression<IScalar>> arguments
            ) 
            where T: ISqlTypeTag
            =>
            new CompoundExpression<T>(
                sqlBuilder,
                SqlPart.Concat(
                    new[] { SqlPart.FromToken(functionName) }
                    .Concat(
                        arguments
                        .Intersperse(SqlPart.FromToken(","))
                        .Enclose(SqlPart.FromToken("("), SqlPart.FromToken(")"))
                    )));
        #endregion

        #region Cast operators
        internal static SqlExpression<Y>
            ForceCast<X, Y>(this SqlExpression<X> expression)
            where X : ISqlTypeTag
            where Y : ISqlTypeTag
        {
            return
                new CompoundExpression<Y>(
                    expression.SqlBuilder,
                    new ConcreteSqlPart(expression.Tokens, expression.Parameters)
                );
        }

        public static SqlExpression<IScalar>
            Box<X>(this SqlExpression<IScalar<X>> expression)
        {
            return expression.ForceCast<IScalar<X>, IScalar>();
        }

        public static SqlExpression<IScalar<X>>
            Unbox<X>(this SqlExpression<IScalar> expression)
        {
            return expression.ForceCast<IScalar, IScalar<X>>();
        }

        public static SqlExpression<IRelation>
            Box<X>(this SqlExpression<IRelation<X>> expression)
        {
            return expression.ForceCast<IRelation<X>, IRelation>();
        }
        #endregion

        #region Normal operators
        public static SqlExpression<IScalar<string>>
            Concat(
                this SqlExpression<IScalar<string>> lhs,
                params SqlExpression<IScalar<string>>[] rhs
            ) =>
            Invoke<IScalar<string>>(lhs.SqlBuilder, "concat", new[] { lhs.Box() }.Concat(rhs.Select(Box)));
        #endregion

        #region Condition operators
        public static SqlCondition
            IsNull<X>(this SqlExpression<IScalar<X>> lhs) =>
            new AtomicSqlCondition(
                lhs.SqlBuilder,
                lhs.Concat(SqlPart.FromToken("is null"))
            );

        public static SqlCondition
            Equal<X>(
                this SqlExpression<IScalar<X>> lhs,
                SqlExpression<IScalar<X>> rhs
            ) =>
            new AtomicSqlCondition(
                lhs.SqlBuilder,
                lhs.Concat(SqlPart.FromToken("=")).Concat(rhs)
            );
        #endregion

        #region Quantification
        static SqlExpression<IScalar<X>> Quantify<X>(
            this SqlExpression<IRelation> relation,
            string quantifier
        )
        {
            var part = SqlPart.FromToken(quantifier).Concat(relation);
            return new CompoundExpression<IScalar<X>>(relation.SqlBuilder, part);
        }

        public static SqlExpression<IScalar<X>> Any<X>(this SqlExpression<IRelation<X>> relation)
        {
            return relation.Box().Quantify<X>("any");
        }

        public static SqlExpression<IScalar<X>> All<X>(this SqlExpression<IRelation<X>> relation)
        {
            return relation.Box().Quantify<X>("all");
        }
        #endregion
    }
}
