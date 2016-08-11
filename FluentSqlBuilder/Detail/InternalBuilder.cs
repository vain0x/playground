using FluentSqlBuilder.Provider;

namespace FluentSqlBuilder.Detail
{
    public abstract class InternalBuilder
    {
        protected SqlBuilder SqlBuilder { get; }

        protected DbProvider Provider
        {
            get { return SqlBuilder.Provider; }
        }
        
        public InternalBuilder(SqlBuilder sqlBuilder)
        {
            SqlBuilder = sqlBuilder;
        }
    }
}
