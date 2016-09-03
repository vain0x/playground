using System;
using System.Collections.Generic;
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
            Action<IRecord> setter
        )
        {
            var columns = table.Columns.ToArray();

            var parameters =
                columns
                .Select(column => sqlBuilder.CreateParameter(column.UniqueName, column.DbType))
                .ToArray();

            var record = new DbParameterRecord();
            foreach (var parameter in parameters)
            {
                record[parameter.ParameterName] = parameter;
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
    }
}
