using System;
using System.Collections.Generic;
using System.Data.Common;

namespace FluentSqlBuilder.Detail
{
    public class SelectBuilder
        : ISqlPart
        , ISqlExecutable
    {
        SqlBuilder SqlBuilder { get; }
        SelectStatement Statement { get; }

        public SelectBuilder(SqlBuilder sqlBuilder, SelectStatement statement)
        {
            SqlBuilder = sqlBuilder;
            Statement = statement;
        }

        #region ISqlPart
        public IEnumerable<string> Tokens => Statement.Tokens.Enclose("(", ")");
        public IEnumerable<DbParameter> Parameters => Statement.Parameters;
        #endregion

        #region ISqlExecutable
        public DbCommand ToCommand()
        {
            return SqlBuilder.CreateCommand(Statement);
        }
        #endregion

        #region Field
        public SelectBuilder Field(SqlExpression expression)
        {
            Statement.Fields.Add(expression);
            return this;
        }
        #endregion

        #region Union
        public FromlessSelectBuilder Union()
        {
            throw new NotImplementedException();
        }
        #endregion
    }
}
