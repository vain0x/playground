using FluentSqlBuilder.Interface;

namespace FluentSqlBuilder.Detail
{
    public abstract class InternalBuilder
    {
        protected SqlBuilder SqlBuilder { get; private set; }

        protected DbProvider DbProvider
        {
            get { return SqlBuilder.Provider; }
        }
        
        public InternalBuilder(SqlBuilder sqlBuilder)
        {
            SqlBuilder = SqlBuilder;
        }
    }
}
