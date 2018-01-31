using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using MySql.Data.MySqlClient;

namespace VainZero.Sandbox
{
    public sealed class Program
    {
        static MySqlConnection Connect()
        {
            var connectionString =
                new MySqlConnectionStringBuilder()
                {
                    Server = "localhost",
                    Database = "sandbox",
                    UserID = "root",
                    Password = "root",
                }.ToString();
            var connection = new MySqlConnection(connectionString);
            connection.Open();
            return connection;
        }

        readonly MySqlConnection connection;

        void Show(MySqlTransaction transaction)
        {
            var query = "select PersonId, Name from people where Name like 't%'";

            using (var command = new MySqlCommand(query, connection, transaction))
            using (var reader = command.ExecuteReader())
            {
                while (reader.Read())
                {
                    Console.WriteLine("Id = {0}, Name = {1}", reader.GetInt64("PersonId"), reader.GetString("Name"));
                }
            }

        }

        public void Run()
        {
            using (var transaction = connection.BeginTransaction())
            {
                using (var command = new MySqlCommand("insert into people (Name) values ('tanaka')", connection, transaction))
                {
                    Console.WriteLine("{0}", command.ExecuteNonQuery());
                }

                using (var command = new MySqlCommand("select last_insert_id() from people", connection, transaction))
                {
                    Console.WriteLine("Id = {0}", command.ExecuteScalar());
                }

                Show(transaction);
            }

            using (var transaction = connection.BeginTransaction())
            {
                Show(transaction);
            }
        }

        public Program(MySqlConnection connection)
        {
            this.connection = connection;
        }

        public static void Main(string[] args)
        {
            using (var connection = Connect())
            {
                new Program(connection).Run();
            }
        }
    }
}
