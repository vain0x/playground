using System.Data;
using FluentSqlBuilder.SqlSyntax;

namespace FluentSqlBuilder.Accessor
{
    public interface IColumn
    {
        string QualifiedName { get; }
        string UniqueName { get; }
        string RawName { get; }
        DbType DbType { get; }
    }

    public abstract class Column<TValue>
        : ScalarSqlExpression<TValue>
        , IColumn
    {
        internal Column(SqlBuilder sqlBuilder)
            : base(sqlBuilder)
        {
        }

        public abstract string QualifiedName { get; }
        public abstract string UniqueName { get; }
        public abstract string RawName { get; }

        public virtual DbType DbType
        {
            get
            {
                return SqlBuilder.Provider.DbTypeMap.DbTypeFromType(typeof(TValue));
            }
        }

        public virtual TValue this[DataRow row]
        {
            get
            {
                return row.Field<TValue>(UniqueName);
            }

            set
            {
                row.SetField(UniqueName, value);
            }
        }

        public virtual TValue this[IValueRecord record]
        {
            get
            {
                return (TValue)record[UniqueName];
            }
            set
            {
                record[UniqueName] = value;
            }
        }

        public virtual ScalarSqlExpression<TValue> this[IExpressionRecord record]
        {
            get
            {
                return record[UniqueName].Unbox<TValue>();
            }
            set
            {
                record[UniqueName] = value;
            }
        }
    }
}
