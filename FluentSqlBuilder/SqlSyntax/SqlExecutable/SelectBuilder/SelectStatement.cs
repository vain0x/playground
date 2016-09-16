using System.Collections.Generic;
using System.Data.Common;
using System.Diagnostics;
using System.Linq;
using Optional;

namespace FluentSqlBuilder.SqlSyntax
{
    sealed class SelectStatement
        : SqlExpression<IRelation>
        , ISqlExecutable
    {
        Option<CombinedSelectStatement> Combined { get; }

        public SqlExpression<IRelation> Source { get; private set; }

        public ConditionBuilder WhereCondition { get; }

        public ConditionBuilder HavingCondition { get; }

        public List<SqlExpression<IScalar>> GroupKeys { get; } =
            new List<SqlExpression<IScalar>>();

        public List<OrderKey> OrderKeys { get; } =
            new List<OrderKey>();

        public List<SqlPart> Fields { get; } =
            new List<SqlPart>();

        public SelectStatement(
            SqlBuilder sqlBuilder,
            Option<CombinedSelectStatement> combined,
            SqlExpression<IRelation> relation
        )
            : base(sqlBuilder)
        {
            Combined = combined;
            Source = relation;
            WhereCondition = new ConditionBuilder(SqlBuilder);
            HavingCondition = new ConditionBuilder(SqlBuilder);
        }

        #region Tokens
        IEnumerable<SqlToken> FieldListTokens =>
            Fields.Intersperse(SqlPart.FromString(","))
            .SelectMany(p => p.Tokens);

        IEnumerable<SqlToken> FromTokens =>
            new[] { SqlToken.FromString("from") }
            .Concat(Source.Tokens);

        IEnumerable<SqlToken> WhereTokens =>
            WhereCondition.IsTrivial
            ? Enumerable.Empty<SqlToken>()
            :
                new[] { SqlToken.FromString("where") }
                .Concat(WhereCondition.Tokens);

        IEnumerable<SqlToken> GroupByTokens =>
            GroupKeys.Any()
            ?
                new[] { SqlToken.FromString("group by") }
                .Concat(
                    GroupKeys
                        .Intersperse(SqlPart.FromString(","))
                        .SelectMany(p => p.Tokens)
                )
            : Enumerable.Empty<SqlToken>();

        IEnumerable<SqlToken> HavingTokens =>
            HavingCondition.IsTrivial
            ? Enumerable.Empty<SqlToken>()
            :
                new[] { SqlToken.FromString("having") }
                .Concat(HavingCondition.Tokens);

        IEnumerable<SqlToken> OrderByTokens =>
            OrderKeys.Any()
            ?
                new[] { SqlToken.FromString("order by") }
                .Concat(
                    OrderKeys.Select(o => o.Tokens)
                    .Intercalate(new[] { SqlToken.FromString(",") })
                )
            : Enumerable.Empty<SqlToken>();

        IEnumerable<SqlToken> SelectTokens =>
            new[] { SqlToken.FromString("select") }
            .Concat(FieldListTokens)
            .Concat(FromTokens)
            .Concat(WhereTokens)
            .Concat(GroupByTokens)
            .Concat(HavingTokens)
            .Concat(OrderByTokens);

        IEnumerable<SqlToken> CombinedTokens
        {
            get
            {
                foreach (var combined in Combined)
                {
                    foreach (var token in combined.Statement.Tokens) yield return token;
                    yield return SqlToken.FromString(combined.Combinator);
                }
            }
        }

        internal override IEnumerable<SqlToken> Tokens =>
            CombinedTokens.Concat(SelectTokens);
        #endregion

        #region ISqlExecutable
        public DbCommand ToCommand() =>
            SqlBuilder.CreateCommand(Tokens);
        #endregion

        public void Join(Join join)
        {
            Source = new JoinedRelation(Source, join);
        }

        public void AddFieldAll()
        {
            var wildmark = SqlPart.FromString(SqlBuilder.Language.GetWildmark());
            Fields.Add(wildmark);
        }

        public void AddFieldAll(AliasedSqlExpression<IRelation> relation)
        {
            var wildmark = SqlBuilder.Language.BuildWildmark(relation.Alias);
            Fields.Add(SqlPart.FromString(wildmark));
        }

        public SqlExpression<IScalar<X>> ToScalar<X>()
        {
            Debug.Assert(Fields.Count == 1);
            return new ConcreteSqlExpression<IScalar<X>>(SqlBuilder, this.Enclose("(", ")"));
        }

        internal SqlExpression<IScalar<X>> Quantify<X>(string quantifier)
        {
            Debug.Assert(Fields.Count == 1);
            var part = SqlPart.FromString(quantifier).Concat(this.Enclose("(", ")"));
            return new ConcreteSqlExpression<IScalar<X>>(SqlBuilder, part);
        }

        public SqlExpression<IRelation> ToRelation()
        {
            Debug.Assert(Fields.Any());
            return new ConcreteSqlExpression<IRelation>(SqlBuilder, this.Enclose("(", ")"));
        }
    }
}
