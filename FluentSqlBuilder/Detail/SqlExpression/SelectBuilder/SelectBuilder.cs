﻿using System;
using System.Collections.Generic;
using System.Data.Common;

namespace FluentSqlBuilder.Detail
{
    public class SelectBuilder
        : ISqlPart
        , ISqlExecutable
    {
        SelectStatement Statement { get; }

        public SelectBuilder(SelectStatement statement)
        {
            Statement = statement;
        }

        #region ISqlPart
        public IEnumerable<string> Tokens => Statement.Tokens.Enclose("(", ")");
        public IEnumerable<DbParameter> Parameters => Statement.Parameters;
        #endregion

        #region ISqlExecutable
        public DbCommand ToCommand() => Statement.ToCommand();
        #endregion

        #region Field
        public SelectBuilder Field<X>(SqlExpression<IScalar<X>> expression)
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
