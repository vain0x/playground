using FluentSqlBuilder.Interface.Detail;

namespace FluentSqlBuilder.Interface
{
    public interface ISelectBuilder
    {
        IOptionallyAliasedBuilder<ISelectBuilder> Field(string expression);

        IFromlessSelectBuilder Union();
    }

    /// <summary>
    /// フィールドリストが空の select 文を表します。
    /// </summary>
    public interface IFieldlessSelectBuilder
    {
        IOptionallyAliasedBuilder<IFieldlessSelectBuilder> From(string qualifier, string tableName);
        IOptionallyAliasedBuilder<IFieldlessSelectBuilder> From(string tableName);

        IOptionallyAliasedBuilder<IJoinClauseBuilder<IFieldlessSelectBuilder>> Join(string tableName);

        IConditionBuilder<IFieldlessSelectBuilder> Where();
        IFieldlessSelectBuilder Where(IConditionBuilder condition);

        IFieldlessSelectBuilder GroupBy(string expression);
        IFieldlessSelectBuilder OrderBy(string expression);

        IConditionBuilder<IFieldlessSelectBuilder> Having();
        IFieldlessSelectBuilder Having(IConditionBuilder condition);

        IOptionallyAliasedBuilder<ISelectBuilder> Field(string expression);
    }

    /// <summary>
    /// from 句が不足している select 文を表します。
    /// </summary>
    public interface IFromlessSelectBuilder
    {
        IOptionallyAliasedBuilder<IFieldlessSelectBuilder> From(string qualifier, string tableName);
        IOptionallyAliasedBuilder<IFieldlessSelectBuilder> From(string tableName);
    }
}
