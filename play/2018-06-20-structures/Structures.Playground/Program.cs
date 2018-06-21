using System;
using System.Diagnostics;

namespace Structures.Playground
{
    using P = Person;

    public sealed class Person
        : RecordBase<Person>
    {
        public static Field<string> Name { get; } = F;
        public static Field<DateTime> Birthday { get; } = F;

        public static Person Create(
            string name,
            DateTime birthday
        )
        {
            var r = new Person();
            return CreateBuilder()
                .Set(Name, name)
                .Set(Birthday, birthday)
                .IntoRecord();
        }
    }

    public class RecordTests
    {
        public void ItCanConstruct()
        {
            var person = P.Create("Hatsune Miku", new DateTime(2007, 8, 31));

            Debug.WriteLine(person.Get(P.Name));
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            new RecordTests().ItCanConstruct();
            Console.WriteLine("Hello World!");
        }
    }
}
