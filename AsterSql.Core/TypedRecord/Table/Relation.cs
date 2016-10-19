using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using AsterSql.SqlSyntax;

namespace AsterSql.TypedRecord
{
    /// <summary>
    /// A base class of all relation classes.
    /// </summary>
    public abstract class Relation
    {
        #region Reflection
        static bool IsColumnType(Type type)
        {
            return type.GetInterface(nameof(IColumn)) != null;
        }

        internal static IEnumerable<PropertyInfo> ColumnProperties(Type relationType)
        {
            return
                relationType
                .GetProperties()
                .Where(propertyInfo => IsColumnType(propertyInfo.PropertyType));
        }

        IReadOnlyList<IColumn> GetColumns()
        {
            return
                ColumnProperties(GetType())
                .Select(p => (IColumn)p.GetValue(this))
                .ToArray();
        }

        Lazy<IReadOnlyList<IColumn>> LazyColumns { get; }

        internal IReadOnlyList<IColumn> Columns => LazyColumns.Value;
        #endregion

        internal static InvalidOperationException NoColumnException =>
            new InvalidOperationException("A relation class must contain at least one property of Column<T>. (リレーションクラスは、Column<T> 型のプロパティを少なくとも1つ含む必要がある。)");

        public Relation()
        {
            LazyColumns = Lazy.Create(() => GetColumns());
        }
    }
}
