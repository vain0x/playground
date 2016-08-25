using System.Data;
using FluentSqlBuilder.Detail;

namespace FluentSqlBuilder.Public
{
    public interface IColumn<TValue>
        : ISqlExpression<IScalar<TValue>>
    {
        string QualifiedName { get; }
        string RawName { get; }

        TValue this[DataRow row] { get; set; }
    }
}
