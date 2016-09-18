using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;
using FluentSqlBuilder.SqlSyntax;

namespace FluentSqlBuilder.Accessor
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

        public Relation()
        {
            LazyColumns = Lazy.Create(() => GetColumns());
        }
    }
}
