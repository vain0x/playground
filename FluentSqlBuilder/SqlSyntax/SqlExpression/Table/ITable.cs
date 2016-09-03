using FluentSqlBuilder.Detail;

namespace FluentSqlBuilder.Public
{
    public interface ITable
        : IAliasedSqlExpression<IRelation>
    {
        string RawName { get; }
        string QualifiedName { get; }

        IColumn<X> Column<X>(string columnName);
    }
}
