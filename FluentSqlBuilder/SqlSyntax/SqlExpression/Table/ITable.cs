using FluentSqlBuilder.Detail;

namespace FluentSqlBuilder.Public
{
    public interface ITable
        : INamedSqlExpression<IRelation>
    {
        string QualifiedName { get; }

        IColumn<X> Column<X>(string columnName);
    }
}
