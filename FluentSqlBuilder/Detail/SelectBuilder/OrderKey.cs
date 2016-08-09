namespace FluentSqlBuilder.Detail
{
    public class OrderKey
    {
        public Expression Expression { get; private set; }
        public OrderDirection Direction { get; private set; }

        public OrderKey(Expression expression, OrderDirection direction)
        {
            Expression = expression;
            Direction = direction;
        }
    }
}
