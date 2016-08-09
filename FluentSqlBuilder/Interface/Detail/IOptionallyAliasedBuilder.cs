namespace FluentSqlBuilder.Interface.Detail
{
    public interface IOptionallyAliasedBuilder<TBase>
        : TBase
    {
        public TBase As(string name);
    }
}
