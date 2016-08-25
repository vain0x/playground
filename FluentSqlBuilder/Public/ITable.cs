using FluentSqlBuilder.Detail;

namespace FluentSqlBuilder.Public
{
    public interface ITable
        : ISqlExpression<IRelation>
    {
        string QualifiedName { get; }
        string RawName { get; }

        IColumn<X> Column<X>(string columnName);
    }
}
