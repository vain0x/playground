using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public class EmptyClass
{
}

public class ClassWithoutCompleteConstructor
{
    public int Value { get; }
}

public class ClassWithUnmarkedConstructor
{
    // The order is changed.

    public int Age { get; }
    public string Name { get; }

    public ClassWithUnmarkedConstructor(string name, int age)
    {
        Name = name ?? "";
        Age = age;
    }
}
