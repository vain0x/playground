using System.Collections.Generic;
using System.Data.Common;

namespace FluentSqlBuilder.Detail
{
    public class OptionallyAliasedExpression
        : SqlExpression
    {
        public SqlExpression Expression { get; }

        public string AliasOrNull { get; set; }
        
        public OptionallyAliasedExpression(SqlExpression expression)
        {
            Expression = expression;
        }

        #region ISqlPart
        public override IEnumerable<string> Tokens
        {
            get
            {
                if (AliasOrNull == null)
                {
                    foreach(var token in Expression.Tokens)
                    {
                        yield return token;
                    }
                }
                else
                {
                    yield return "(";
                    foreach(var token in Expression.Tokens)
                    {
                        yield return token;
                    }
                    yield return ")";
                    yield return "as";
                    yield return AliasOrNull;
                }
            }
        }

        public override IEnumerable<DbParameter> Parameters =>
            Expression.Parameters;
        #endregion
    }
}
