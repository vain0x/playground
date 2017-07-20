using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using VainZero.EntityClassGenerator.Domain;

namespace VainZero.EntityClassGenerator.Providers
{
    public sealed class MySqlTypeNameMapper
        : ITypeNameMapper
    {
        public string CSharpTypeName(DbColumn column)
        {
            var typeName = column.TypeName;
            var numericPrecision = column.NumericPrecision;
            var question = column.IsNullable ? "?" : "";

            switch (typeName)
            {
                case "tinyint":
                case "bit":
                    if (numericPrecision == 1)
                    {
                        return "bool" + question;
                    }
                    else
                    {
                        return "sbyte" + question;
                    }
                case "smallint":
                    return "sbyte" + question;
                case "mediumint":
                    return "short" + question;
                case "int":
                    {
                        var columnName = column.Name.ToLower();
                        if (columnName == "id" || columnName.EndsWith("_id"))
                        {
                            return "long" + question;
                        }
                    }
                    return "int" + question;
                case "bigint":
                    return "long" + question;
                case "double":
                    return "double" + question;
                case "decimal":
                    return "decimal" + question;
                case "enum":
                case "char":
                case "varchar":
                case "nvarchar":
                case "text":
                case "tinytext":
                case "mediumtext":
                case "longtext":
                    return "string";
                case "date":
                case "datetime":
                case "timestamp":
                    return "DateTime" + question;
                case "time":
                    return "TimeSpan" + question;
                case "varbinary":
                case "blob":
                case "tinyblob":
                case "mediumblob":
                case "longblob":
                    return "byte[]";
                default:
                    throw new InvalidOperationException($"Unknown data type: {typeName}.");
            }
        }
    }
}
