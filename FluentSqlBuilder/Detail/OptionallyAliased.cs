namespace FluentSqlBuilder.Detail
{
    public class OptionallyAliased<TValue>
    {
        public TValue Value { get; private set; }

        public string AliasOrNull { get; set; } = null;
        
        public OptionallyAliased(TValue value)
        {
            Value = value;
        }
    }
}
