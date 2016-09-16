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
        : SqlPart
    {
        public JoinType JoinType { get; }
        public SqlExpression<IRelation> Relation { get; }

        public Join(
            JoinType joinType,
            SqlExpression<IRelation> relation
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
    }

    public sealed class JoinOn
        : Join
    {
        public SqlCondition Condition { get; }

        public JoinOn(
            JoinType joinType,
            SqlExpression<IRelation> relation,
            SqlCondition condition
        )
            : base(joinType, relation)
        {
            Condition = condition;
        }

        internal override IEnumerable<string> Tokens
        {
            get
            {
                yield return JoinWord;
                foreach (var token in Relation.Tokens) yield return token;
                yield return "on";
                foreach (var token in Condition.Tokens) yield return token;
            }
        }

        internal override IEnumerable<DbParameter> Parameters =>
            Relation.Parameters.Concat(Condition.Parameters);
    }

    public sealed class JoinUsing
        : Join
    {
        public string Column { get; }

        public JoinUsing(
            JoinType joinType,
            SqlExpression<IRelation> relation,
            string column
        )
            : base(joinType, relation)
        {
            Column = column;
        }

        internal override IEnumerable<string> Tokens
        {
            get
            {
                yield return JoinWord;
                foreach (var token in Relation.Tokens) yield return token;
                yield return "using";
                yield return "(";
                yield return Column;
                yield return ")";
            }
        }

        internal override IEnumerable<DbParameter> Parameters =>
            Relation.Parameters;
    }
}
