namespace FluentSqlBuilder.Interface
{
    public interface ISqlBuilder
    {
        IConditionBuilder And();
        IConditionBuilder Or();

        IFromlessSelectBuilder Select();
    }
}
