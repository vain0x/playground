using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Data.Common;
using System.Data.Entity;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.Sandbox
{
    public class Person
    {
        [Key]
        [Column]
        public long Id { get; set; }

        [Column]
        [StringLength(1024)]
        public string Name { get; set; }
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
        public string Do(int type)
        {
            using (var context = new MyContext())
            {
                // Add records.
                if (type % 2 == 0)
                {
                    context.Persons.Add(new Person() { Name = "Miku" });
                    context.Persons.Add(new Person() { Name = "Rin" });
                    context.Persons.Add(new Person() { Name = "Luka" });
                }
                else
                {
                    context.Persons.Add(new Person() { Name = "GUMI" });
                    context.Persons.Add(new Person() { Name = "Gakupo" });
                }
                context.SaveChanges();

                // Gets all records.
                var persons = context.Persons.ToArray();
                return string.Join(", ", persons.Select(p => p.Name));
            }
        }

        public void Run()
        {
            var tasks =
                Enumerable.Range(0, 100)
                .Select(i => Task.Run(() => Do(i)))
                .ToArray();

            foreach (var result in Task.WhenAll(tasks).Result)
            {
                Console.WriteLine(result);
            }
        }

        public static void Main(string[] args)
        {
            new Program().Run();
        }
    }
}
