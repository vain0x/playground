namespace FluentSqlBuilder.Detail
{
    public class OrderKey
    {
        public Expression Expression { get; }
        public OrderDirection Direction { get; }

        public OrderKey(Expression expression, OrderDirection direction)
        {
            Expression = expression;
            Direction = direction;
        }
    }
}
