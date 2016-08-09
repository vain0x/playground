namespace FluentSqlBuilder.Implementation.Detail
{
    public class OptionallyAliased<TValue>
    {
        public TValue Value { get; private set; }
        public string AliasOrNull { get; private set; }

        public OptionallyAliased(TValue value, string aliasOrNull)
        {
            Value = value;
            AliasOrNull = aliasOrNull;
        }

        public OptionallyAliased(TValue value)
            : this(value, null)
        {
        }
    }
}
