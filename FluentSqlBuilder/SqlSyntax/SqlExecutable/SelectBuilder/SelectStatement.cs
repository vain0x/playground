using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public class SelectStatement
        : SqlExpression<IRelation>
        , IRelationalQueryOrCommand
        , ISqlExecutable
    {
        public JoinedRelation Source { get; } =
            new JoinedRelation();

        public ConditionBuilder WhereCondition { get; }

        public ConditionBuilder HavingCondition { get; }

        public List<ISqlPart> GroupKeys { get; } =
            new List<ISqlPart>();

        public List<OrderKey> OrderKeys { get; } =
            new List<OrderKey>();

        public List<ISqlPart> Fields { get; } =
            new List<ISqlPart>();

        public SelectStatement(SqlBuilder sqlBuilder)
            : base(sqlBuilder)
        {
            WhereCondition = new ConditionBuilder(SqlBuilder);
            HavingCondition = new ConditionBuilder(SqlBuilder);
        }

        #region SqlExpression
        public override IEnumerable<string> Tokens
        {
            get
            {
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
            .Concat(WhereCondition.Parameters)
            .Concat(HavingCondition.Parameters)
            .Concat(GroupKeys.SelectMany(g => g.Parameters))
            .Concat(OrderKeys.SelectMany(o => o.Parameters))
            .Concat(Fields.SelectMany(f => f.Parameters));
        #endregion

        public DbCommand ToCommand() =>
            SqlBuilder.CreateCommand(this);
    }
}
