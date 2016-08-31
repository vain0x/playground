using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Linq;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public enum JoinType
    {
        Inner,
        Cross,
        LeftOuter,
        RightOuter,
        FullOuter,
    }

    public abstract class Join
        : ISqlPart
    {
        public JoinType JoinType { get; }
        public ISqlExpression<IRelation> Relation { get; }

        public Join(
            JoinType joinType,
            ISqlExpression<IRelation> relation
        )
        {
            JoinType = joinType;
            Relation = relation;
        }

        public string JoinWord
        {
            get
            {
                switch (JoinType)
                {
                    case JoinType.Inner: return "join";
                    case JoinType.Cross: return "cross join";
                    case JoinType.LeftOuter: return "left outer join";
                    case JoinType.RightOuter: return "right outer join";
                    case JoinType.FullOuter: return "full outer join";
                    default: throw new Exception();
                }
            }
        }

        #region ISqlPart
        public abstract IEnumerable<string> Tokens { get; }
        public abstract IEnumerable<DbParameter> Parameters { get; }
        #endregion
    }

    public class JoinOn
        : Join
    {
        public ConditionBuilder Condition { get; }

        public JoinOn(
            JoinType joinType,
            ISqlExpression<IRelation> relation,
            ConditionBuilder condition
        )
            : base(joinType, relation)
        {
            Condition = condition;
        }

        public override IEnumerable<string> Tokens
        {
            get
            {
                yield return JoinWord;
                foreach (var token in Relation.Tokens) yield return token;
                yield return "on";
                foreach (var token in Condition.Tokens) yield return token;
            }
        }

        public override IEnumerable<DbParameter> Parameters =>
            Relation.Parameters.Concat(Condition.Parameters);
    }

    public class JoinUsing
        : Join
    {
        public ISqlExpression<IScalar<object>> Column { get; }

        public JoinUsing(
            JoinType joinType,
            ISqlExpression<IRelation> relation,
            ISqlExpression<IScalar<object>> column
        )
            : base(joinType, relation)
        {
            Column = column;
        }

        public override IEnumerable<string> Tokens
        {
            get
            {
                yield return JoinWord;
                foreach (var token in Relation.Tokens) yield return token;
                yield return "(";
                foreach (var token in Column.Tokens) yield return token;
                yield return ")";
            }
        }

        public override IEnumerable<DbParameter> Parameters =>
            Relation.Parameters.Concat(Column.Parameters);
    }
}
