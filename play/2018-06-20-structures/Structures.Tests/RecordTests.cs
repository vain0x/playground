using System;
using System.Linq;
using Xunit;

namespace Structures
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
            return CreateBuilder()
                .Set(Name, name)
                .Set(Birthday, birthday)
                .IntoRecord();
        }
    }

    public class RecordTests
    {
        [Fact]
        public void ItCanConstruct()
        {
            var r = new Person();
            var person = P.Create("Hatsune Miku", new DateTime(2007, 8, 31));

            person.Get(P.Name).Is("Hatsune Miku");
            person.Get(P.Birthday).ToString("yyyy-MM-dd").Is("2007-08-31");
        }
    }
}
