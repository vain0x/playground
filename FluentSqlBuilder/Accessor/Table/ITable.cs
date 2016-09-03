using System;
using System.Data.Common;
using FluentSqlBuilder.Detail;

namespace FluentSqlBuilder.Public
{
    public interface ITable
        : IAliasedSqlExpression<IRelation>
    {
        string RawName { get; }

        IColumn<X> Column<X>(string columnName);
    }
}
