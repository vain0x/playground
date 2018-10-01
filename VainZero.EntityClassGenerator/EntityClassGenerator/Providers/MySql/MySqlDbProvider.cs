using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using MySql.Data.MySqlClient;
using VainZero.EntityClassGenerator.Domain;

namespace VainZero.EntityClassGenerator.Providers
{
    public sealed class MySqlDbProvider
        : IDbProvider
    {
        string ConnectionString { get; }

        public ITypeNameMapper TypeNameMapper { get; } = new MySqlTypeNameMapper();

        #region GetSchema
        static IReadOnlyList<string> TableNames(MySqlConnection connection)
        {
            var tableNames = new List<string>();

            using (var command = new MySqlCommand("show tables", connection))
            using (var reader = command.ExecuteReader())
            {
                while (reader.Read())
                {
                    tableNames.Add(reader.GetString(0));
                }
            }

            return tableNames;
        }

        static DbSchema GetSchemaCore(MySqlConnection connection)
        {
            var list = new List<DbColumn>();

            using (var schema = connection.GetSchema("Columns"))
            {
                foreach (var row in schema.Select())
                {
                    var column =
                        new DbColumn(
                            row.Field<string>("TABLE_NAME"),
                            row.Field<string>("COLUMN_NAME"),
                            row.Field<string>("IS_NULLABLE") != "NO",
                            row.Field<string>("DATA_TYPE"),
                            (int?)row.Field<ulong?>("CHARACTER_MAXIMUM_LENGTH"),
                            (int?)row.Field<ulong?>("NUMERIC_PRECISION"),
                            (int?)row.Field<ulong?>("NUMERIC_SCALE"),
                            row.Field<string>("COLUMN_DEFAULT"),
                            row.Field<string>("COLUMN_KEY") == "PRI",
                            row.Field<string>("EXTRA") == "auto_increment",
                            (int)row.Field<ulong>("ORDINAL_POSITION")
                        );
                    list.Add(column);
                }
            }

            var tables =
                list
                .Where(column => !column.TableName.StartsWith("__"))
                .GroupBy(column => column.TableName)
                .Select(g => new DbTable(g.Key, g.OrderBy(column => column.Priority).ToArray()))
                .ToArray();
            return new DbSchema(tables);
        }

        public DbSchema GetSchema()
        {
            using (var connection = new MySqlConnection(ConnectionString))
            {
                connection.Open();
                return GetSchemaCore(connection);
            }
        }
        #endregion

        public MySqlDbProvider(string connectionString)
        {
            ConnectionString = connectionString;
        }
    }
}
