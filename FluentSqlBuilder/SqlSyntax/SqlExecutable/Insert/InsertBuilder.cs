using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Data.Common;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Detail
{
    public static class InsertBuilder
    {
        public static DbCommand InsertValuesCommand(
            SqlBuilder sqlBuilder,
            Table table,
            Action<IValueRecord> setter
        )
        {
            var columns = table.Columns.Value;

            var parameters =
                columns
                .Select(column => sqlBuilder.CreateParameter(column.UniqueName, column.DbType))
                .ToArray();

            var record = new DbParameterRecord();
            foreach (var parameter in parameters)
            {
                record.Add(parameter.ParameterName, parameter);
            }

            setter(record);

            var columnNameList = table.ColumnNameList.Value;
            var parameterList = table.ColumnUniqueNameParameterList.Value;

            var dbCommand = sqlBuilder.Provider.Factory.CreateCommand();
            dbCommand.CommandText =
                $"insert into {table.QuotedName} ({columnNameList}) values ({parameterList})";
            dbCommand.Parameters.AddRange(parameters);
            return dbCommand;
        }

        public static DbCommand InsertSelectCommand(
            SelectStatement selectStatement,
            Table table,
            Action<IExpressionRecord> setter
        )
        {
            var sqlBuilder = selectStatement.SqlBuilder;
            var columns = table.Columns.Value;

            var assignment = new AssignmentRecord();
            foreach (var column in columns)
            {
                assignment.Add(column.UniqueName, null);
            }

            setter(assignment);

            if (assignment.Values.Contains(null))
            {
                var keys = assignment.Where(kv => kv.Value == null).Select(kv => kv.Key);
                throw new Exception($"No expressions assigned to: {keys.Intercalate(',')}");
            }

            Debug.Assert(selectStatement.Fields.IsEmpty());
            selectStatement.Fields.AddRange(columns.Select(c => assignment[c.UniqueName]));

            var tokens =
                new[]
                {
                    SqlToken.FromString("insert"),
                    SqlToken.FromString("into"),
                    SqlToken.FromString(table.QuotedName),
                    SqlToken.FromString($"({table.ColumnNameList.Value})")
                }
                .Concat(selectStatement.Tokens);
            return sqlBuilder.CreateCommand(tokens);
        }
    }
}
