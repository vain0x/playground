using Microsoft.Data.Sqlite;

namespace AppCore
{
    public sealed class AppDatabase
    {
        static AppDatabase()
        {
            SQLitePCL.Batteries.Init();
        }

        public static async Task Example()
        {
            using var cx = new SqliteConnection("Data Source=app.db");
            await cx.OpenAsync();

            var schema = await cx.GetSchemaAsync();
            {
                {
                    var command = cx.CreateCommand();
                    command.CommandText = "CREATE TABLE example_records(id BIGINT PRIMARY KEY, value STRING);";
                    await command.ExecuteNonQueryAsync();
                }
                {
                    var command = cx.CreateCommand();
                    command.CommandText = "INSERT INTO example_records values ($id, $value);";
                    command.Parameters.AddWithValue("$id", 1);
                    command.Parameters.AddWithValue("$value", "Hello, sqlite!");
                    var status = await command.ExecuteNonQueryAsync();
                    Console.WriteLine($"insert result = {status}");
                }
            }

            {
                var command = cx.CreateCommand();
                command.CommandText = "SELECT id, value FROM example_records ORDER BY id";

                using var reader = await command.ExecuteReaderAsync();
                while (reader.Read())
                {
                    var id = reader.GetInt32(0);
                    var value = reader.GetString(1);
                    Console.WriteLine($"id: {id}, value: '{value}'");
                }
            }
        }
    }
}
