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
using SQLite.CodeFirst;
using System.Data.Common;
using System.Data;
using System.Data.Entity.Core.EntityClient;
using System.Data.Entity.Core.Metadata.Edm;

namespace VainZero.Sandbox
{
    public class Person
    {
        [Key]
        [Column]
        public long Id { get; set; }

        [Column]
        [StringLength(512)]
        public string Name { get; set; }
    }

    public class MyContext
        : DbContext
    {
        static DbConnection CreateConnection()
        {
            var connection = new SQLiteConnection(ConnectionString);
            connection.Open();
            return connection;
        }

        MyContext(DbConnection connection)
            : base(connection, contextOwnsConnection: false)
        {
            Database.Log = text => Debug.WriteLine(text);
        }

        public MyContext()
            : this(CreateConnection())
        {
        }

        #region ConnectionString
        const string dbPath = @":memory:";

        static SQLiteConnectionStringBuilder ConnectionStringBuilder =>
            new SQLiteConnectionStringBuilder
            {
                DataSource = dbPath,
                ForeignKeys = true,
                BinaryGUID = false,
            };

        static string ConnectionString { get; } =
            ConnectionStringBuilder.ConnectionString;
        #endregion

        #region OnModelCreating
        class MyDbInitializer
            : SqliteCreateDatabaseIfNotExists<MyContext>
        {
            public MyDbInitializer(DbModelBuilder mb)
                : base(mb, nullByteFileMeansNotExisting: true)
            {
            }

            protected override void Seed(MyContext context)
            {
                base.Seed(context);

                context.Set<Person>().Add(new Person() { Name = "John Doe" });
                context.Set<Person>().Add(new Person() { Name = "vain0" });
                context.SaveChanges();
            }
        }

        protected sealed override void OnModelCreating(DbModelBuilder mb)
        {
            mb.Entity<Person>();

            Database.SetInitializer(new MyDbInitializer(mb));
        }
        #endregion
    }

    public sealed class Program
    {
        public void Run()
        {
            using (var context = new MyContext())
            {
                Console.WriteLine("Start.");
                var persons = context.Set<Person>().ToArray();
                foreach (var person in persons)
                {
                    Console.WriteLine("{0}", person.Name);
                }

                using (var transaction = context.Database.BeginTransaction())
                {
                    context.Set<Person>().Remove(persons[0]);
                    context.Set<Person>().Add(new Person() { Name = "Miku" });

                    Console.WriteLine("Transaction.");

                    foreach (var person in context.Set<Person>())
                    {
                        Console.WriteLine("{0}", person.Name);
                    }

                    Console.WriteLine("Rollback.");
                }

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
