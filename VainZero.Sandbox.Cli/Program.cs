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

        public void Run()
        {
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
