namespace Examples.Ast
{
    using System;

    public partial class Person
    {
        // analyzer: complete-constructor
        public Person(string name, int age)
        {
            if (name == null)
                throw new ArgumentNullException("name");
            Name = name;
            Age = age;
        }
    }

    public partial struct Maybe<T>
    {
        // analyzer: complete-constructor
        public Maybe(bool hasValue, T value)
        {
            HasValue = hasValue;
            Value = value;
        }
    }
}
