namespace FluentSqlBuilder.Detail
{
    public class OptionallyAliased<TValue>
    {
        public TValue Value { get; }

        public string AliasOrNull { get; set; }
        
        public OptionallyAliased(TValue value)
        {
            Value = value;
        }
    }
}
