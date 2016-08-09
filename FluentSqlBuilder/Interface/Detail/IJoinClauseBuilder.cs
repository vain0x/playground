namespace FluentSqlBuilder.Interface.Detail
{
    public interface IJoinClauseBuilder<TResult>
    {
        IConditionBuilder<TResult> On();
        TResult On(IConditionBuilder condition);

        TResult Using(string fieldName);
    }
}
