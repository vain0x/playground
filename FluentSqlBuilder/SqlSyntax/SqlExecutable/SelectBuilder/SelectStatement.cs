using System.Collections.Generic;
using System.Data.Common;
using System.Diagnostics;
using System.Linq;
using Optional;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public class SelectStatement
        : SqlExpression<IRelation>
        , IRelationalQueryOrCommand
        , ISqlExecutable
    {
        Option<CombinedSelectStatement> Combined { get; }

        public JoinedRelation Source { get; } =
            new JoinedRelation();

        public ConditionBuilder WhereCondition { get; }

        public ConditionBuilder HavingCondition { get; }

        public List<ISqlExpression<IScalar>> GroupKeys { get; } =
            new List<ISqlExpression<IScalar>>();

        public List<OrderKey> OrderKeys { get; } =
            new List<OrderKey>();

        public List<ISqlPart> Fields { get; } =
            new List<ISqlPart>();

        public SelectStatement(SqlBuilder sqlBuilder, Option<CombinedSelectStatement> combined)
            : base(sqlBuilder)
        {
            Combined = combined;
            WhereCondition = new ConditionBuilder(SqlBuilder);
            HavingCondition = new ConditionBuilder(SqlBuilder);
        }

        #region SqlExpression
        public override IEnumerable<string> Tokens
        {
            get
            {
                foreach (var combined in Combined)
                {
                    foreach (var token in combined.Statement.Tokens) yield return token;
                    yield return combined.Combinator;
                }

                yield return "select";

                var fieldListTokens = Fields.Select(f => f.Tokens).Intercalate(new[] { "," });
                foreach (var token in fieldListTokens) yield return token;

                yield return "from";
                foreach (var token in Source.Tokens) yield return token;

                if (!WhereCondition.IsTrivial)
                {
                    yield return "where";
                    foreach (var token in WhereCondition.Tokens) yield return token;
                }

                if (GroupKeys.Any())
                {
                    yield return "group by";
                    var tokens = GroupKeys.Select(g => g.Tokens).Intercalate(new[] { "," });
                    foreach (var token in tokens) yield return token;
                }

                if (!HavingCondition.IsTrivial)
                {
                    yield return "having";
                    foreach (var token in HavingCondition.Tokens) yield return token;
                }

                if (OrderKeys.Any())
                {
                    yield return "order by";
                    var tokens = OrderKeys.Select(o => o.Tokens).Intercalate(new[] { "," });
                    foreach (var token in tokens) yield return token;
                }
            }
        }

        public override IEnumerable<DbParameter> Parameters =>
            Source.Parameters
            .Concat(
                Combined.Match(
                    combined => combined.Statement.Parameters,
                    () => Enumerable.Empty<DbParameter>()
                ))
            .Concat(WhereCondition.Parameters)
            .Concat(HavingCondition.Parameters)
            .Concat(GroupKeys.SelectMany(g => g.Parameters))
            .Concat(OrderKeys.SelectMany(o => o.Parameters))
            .Concat(Fields.SelectMany(f => f.Parameters));
        #endregion

        #region ISqlExecutable
        public DbCommand ToCommand() =>
            SqlBuilder.CreateCommand(ToString(), Parameters);
        #endregion

        public void AddFieldAll()
        {
            var wildmark = SqlPart.FromToken(SqlBuilder.Language.GetWildmark());
            Fields.Add(wildmark);
        }

        public void AddFieldAll(IAliasedSqlExpression<IRelation> relation)
        {
            var wildmark = SqlBuilder.Language.BuildWildmark(relation.Alias);
            Fields.Add(SqlPart.FromToken(wildmark));
        }

        public ISqlExpression<IScalar<X>> ToScalar<X>()
        {
            Debug.Assert(Fields.Count == 1);
            return new CompoundExpression<IScalar<X>>(SqlBuilder, this.Enclose("(", ")"));
        }

        internal ISqlExpression<IScalar<X>> Quantify<X>(string quantifier)
        {
            Debug.Assert(Fields.Count == 1);
            var part = SqlPart.FromToken(quantifier).Concat(this.Enclose("(", ")"));
            return new CompoundExpression<IScalar<X>>(SqlBuilder, part);
        }

        public ISqlExpression<IRelation> ToRelation()
        {
            Debug.Assert(Fields.Any());
            return new CompoundExpression<IRelation>(SqlBuilder, this.Enclose("(", ")"));
        }
    }
}
