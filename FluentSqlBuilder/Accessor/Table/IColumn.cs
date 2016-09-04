using System.Data;
using FluentSqlBuilder.Detail;

namespace FluentSqlBuilder.Public
{
    public interface IColumn
    {
        string QualifiedName { get; }
        string UniqueName { get; }
        string RawName { get; }
        DbType DbType { get; }
    }

    public interface IColumn<TValue>
        : IColumn
        , ISqlExpression<IScalar<TValue>>
    {
        TValue this[DataRow row] { get; set; }
        TValue this[IValueRecord record] { get; set; }
        ISqlExpression<IScalar<TValue>> this[IExpressionRecord record] { get; set; }
    }
}
