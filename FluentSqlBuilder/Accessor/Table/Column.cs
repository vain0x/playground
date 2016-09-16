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

    public abstract class Column<TValue>
        : SqlExpression<IScalar<TValue>>
        , IColumn
    {
        protected Column(SqlBuilder sqlBuilder)
            : base(sqlBuilder)
        {
        }

        public abstract TValue this[DataRow row] { get; set; }
        public abstract TValue this[IValueRecord record] { get; set; }
        public abstract SqlExpression<IScalar<TValue>> this[IExpressionRecord record] { get; set; }

        #region IColumn
        public abstract string QualifiedName { get; }
        public abstract string UniqueName { get; }
        public abstract string RawName { get; }
        public abstract DbType DbType { get; }
        #endregion
    }
}
