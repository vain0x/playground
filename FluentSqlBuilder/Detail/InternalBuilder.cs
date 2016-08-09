namespace FluentSqlBuilder.Detail
{
    public abstract class InternalBuilder
    {
        protected SqlBuilder SqlBuilder { get; }

        protected DbmsDialect DbmsDialect
        {
            get { return SqlBuilder.Dialect; }
        }
        
        public InternalBuilder(SqlBuilder sqlBuilder)
        {
            SqlBuilder = SqlBuilder;
        }
    }
}
