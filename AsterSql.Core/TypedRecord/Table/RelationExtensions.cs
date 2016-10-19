using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using AsterSql.Core;
using AsterSql.SqlSyntax;

namespace AsterSql.TypedRecord
{
    /// <summary>
    /// Provides static methods and extension methods related to Relation.
    /// </summary>
    public static class RelationExtensions
    {
        #region Field
        /// <summary>
        /// Adds fields of the relation to the select statement.
        /// </summary>
        /// <typeparam name="R"></typeparam>
        /// <param name="selectBuilder"></param>
        /// <returns></returns>
        public static SelectBuilder Fields(
            this FieldlessSelectBuilder fieldlessSelectBuilder,
            Relation relation
        )
        {
            foreach (var uncons in relation.Columns.UnconsOrNone())
            {
                var firstColumn = uncons.Item1;
                var restColumns = uncons.Item2;

                var selectBuilder = fieldlessSelectBuilder.Field(firstColumn.AsExpression);

                foreach (var column in restColumns)
                {
                    selectBuilder.Field(column.AsExpression);
                }

                return selectBuilder;
            }

            throw Relation.NoColumnException;
        }

        /// <summary>
        /// Add fields of the relation to the select statement.
        /// </summary>
        /// <param name="selectBuilder"></param>
        /// <param name="relation"></param>
        /// <returns></returns>
        public static SelectBuilder Fields(
            this SelectBuilder selectBuilder,
            Relation relation
        )
        {
            foreach (var column in relation.Columns)
            {
                selectBuilder.Field(column.AsExpression);
            }
            return selectBuilder;
        }
        #endregion
    }
}
