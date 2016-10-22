using System.Collections.Generic;
using System.Data.Common;
using System.Diagnostics;
using System.Linq;
using DotNetKit.ErrorHandling;

namespace AsterSql.SqlSyntax
{
    sealed class SelectStatement
        : SqlPart
        , ISqlExecutable
    {
        internal SqlBuilder SqlBuilder { get; }

        Option<CombinedSelectStatement> Combined { get; }

        public RelationSqlExpression Source { get; private set; }

        public CompoundSqlCondition WhereCondition { get; }

        public CompoundSqlCondition HavingCondition { get; }

        public List<ScalarSqlExpression> GroupKeys { get; } =
            new List<ScalarSqlExpression>();

        public List<OrderKey> OrderKeys { get; } =
            new List<OrderKey>();

        public List<SqlPart> Fields { get; } =
            new List<SqlPart>();

        public SelectStatement(
            SqlBuilder sqlBuilder,
            Option<CombinedSelectStatement> combined,
            RelationSqlExpression relation
        )
        {
            SqlBuilder = sqlBuilder;
            Combined = combined;
            Source = relation;
            WhereCondition = new CompoundSqlCondition(SqlBuilder);
            HavingCondition = new CompoundSqlCondition(SqlBuilder);
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
                foreach (var combined in Combined.ToEnumerable())
                {
                    foreach (var token in combined.Statement.Tokens) yield return token;
                    yield return SqlToken.FromString(combined.Combinator);
                }
            }
        }

        internal override IEnumerable<SqlToken> Tokens =>
            CombinedTokens.Concat(SelectTokens);
        #endregion

        public DbCommand ToCommand()
        {
            return SqlBuilder.CreateCommand(Tokens);
        }

        public void Join(Join join)
        {
            Source = new JoinedRelation(Source, join);
        }

        public void AddFieldAll()
        {
            var wildmark = SqlPart.FromString(SqlBuilder.Language.GetWildmark());
            Fields.Add(wildmark);
        }

        public void AddFieldAll(IAliasedSqlExpression relation)
        {
            var wildmark = SqlBuilder.Language.BuildWildmark(relation.Alias);
            Fields.Add(SqlPart.FromString(wildmark));
        }

        public ScalarSqlExpression<X> ToScalar<X>()
        {
            Debug.Assert(Fields.Count == 1);
            return new ConcreteScalarSqlExpression<X>(SqlBuilder, Tokens.Enclose("(", ")"));
        }

        internal ScalarSqlExpression<X> Quantify<X>(string quantifier)
        {
            Debug.Assert(Fields.Count == 1);
            var part = new[] { SqlToken.FromString(quantifier) }.Concat(Tokens.Enclose("(", ")"));
            return new ConcreteScalarSqlExpression<X>(SqlBuilder, part);
        }

        public RelationSqlExpression ToRelation()
        {
            Debug.Assert(Fields.Any());
            return new ConcreteRelationSqlExpression(SqlBuilder, Tokens.Enclose("(", ")"));
        }
    }
}
