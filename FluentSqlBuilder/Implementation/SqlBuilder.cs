using System;
using FluentSqlBuilder.Interface;

namespace FluentSqlBuilder.Implementation
{
    public class SqlBuilder
        : ISqlBuilder
    {
        public IConditionBuilder And()
        {
            throw new NotImplementedException();
        }

        public IConditionBuilder Or()
        {
            throw new NotImplementedException();
        }

        public IFromlessSelectBuilder Select()
        {
            throw new NotImplementedException();
        }
    }
}
