using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Data.Common;
using System.Data.Entity;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Transactions;

namespace VainZero.Sandbox
{
    public class Person
    {
        [Key]
        public long Id { get; set; }

        [StringLength(1024)]
        public string Name { get; set; }

        public bool IsAdult { get; set; }

        public Gender Gender { get; set; } = Gender.Other;
    }

    public enum Gender
    {
        Other,
        Male,
        Female,
    }

    public sealed class MyContext
        : DbContext
    {
        public DbSet<Person> Persons { get; set; }

        public MyContext()
            : base(new Effort.Provider.EffortConnection() { ConnectionString = "Data Source=in-process;IsTransient=true" }, contextOwnsConnection: true)
        {
        }
    }

    public sealed class Program
    {
        public string Name(IEnumerable<Person> persons)
        {
            return string.Join(", ", persons.Select(p => p.Name));
        }

        public MyContext Connect()
        {
            var context = new MyContext();

            context.Persons.Add(new Person() { Name = "Miku", Gender = Gender.Female });
            context.Persons.Add(new Person() { Name = "Rin" });
            context.Persons.Add(new Person() { Name = "Luka", IsAdult = true });
            context.SaveChanges();

            return context;
        }

        void TestInitialization()
        {
            using (var context = Connect())
            {
                var persons = context.Persons.ToArray();
                Debug.Assert(Name(context.Persons) == "Miku, Rin, Luka");

                var miku = persons[0];
                Debug.Assert(miku.Gender == Gender.Female);

                var luka = persons[2];
                Debug.Assert(luka.IsAdult && !miku.IsAdult);
            }
        }

        async Task TestRollbackAsync()
        {
            using (var context = Connect())
            {
                using (var transaction = new TransactionScope(TransactionScopeOption.Required, TransactionScopeAsyncFlowOption.Enabled))
                {
                    await Task.Yield();
                    var persons = await context.Persons.ToArrayAsync();
                    context.Persons.Remove(persons.Last());
                    await context.SaveChangesAsync();

                    context.Persons.Add(new Person() { Name = "GUMI" });
                    await context.SaveChangesAsync();
                    Debug.Assert(Name(context.Persons) == "Miku, Rin, GUMI");
                }

                Debug.Assert(Name(context.Persons) == "Miku, Rin, Luka");
            }
        }

        void TestRollback()
        {
            TestRollbackAsync().Wait();
        }

        public void Run()
        {
            TestInitialization();
            TestRollback();
        }

        public static void Main(string[] args)
        {
            new Program().Run();
        }
    }
}
