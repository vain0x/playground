using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Collections.Generic;
using System.Data.Entity;
using System.Data.Entity.ModelConfiguration.Conventions;
using System.Data.SQLite;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.Sandbox
{
    public class MyContext : DbContext
    {
        const string dbPath = @"./database.sqlite";

        static SQLiteConnectionStringBuilder ConnectionStringBuilder =>
            new SQLiteConnectionStringBuilder
            {
                DataSource = dbPath,
                ForeignKeys = true,
                BinaryGUID = false,
            };

        static string ConnectionString { get; } =
            ConnectionStringBuilder.ConnectionString;

        public MyContext()
            : base(new SQLiteConnection(ConnectionString), contextOwnsConnection: true)
        {
            Database.Log = text => Debug.WriteLine(text);
        }

        protected sealed override void OnModelCreating(DbModelBuilder mb)
        {
            Database.SetInitializer(new SQLite.CodeFirst.SqliteCreateDatabaseIfNotExists<MyContext>(mb));

            mb.Entity<Person>();
        }
    }

    public class Person
    {
        [Key]
        [Column]
        public long Id { get; set; }

        [Column]
        [StringLength(512)]
        public string Name { get; set; }
    }

    public sealed class Program
    {
        public void Run()
        {
            using (var context = new MyContext())
            {
                context.Set<Person>().Add(new Person() { Name = "John Doe" });
                context.Set<Person>().Add(new Person() { Name = "vain0" });
                context.SaveChanges();
            }

            using (var context = new MyContext())
            {
                foreach (var person in context.Set<Person>())
                {
                    Console.WriteLine("{0}", person.Name);
                }
            }
        }

        public static void Main(string[] args)
        {
            new Program().Run();
        }
    }
}
