namespace FluentSqlBuilder.Interface
{
    public interface IConditionBuilder
    {
        IConditionBuilder Equal(string lhs, string rhs);
    }

    public interface IConditionBuilder<TResult>
    {
        TResult Equal(string lhs, string rhs);
    }
}
