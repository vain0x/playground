using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public class Sample
{
    // The type of Age is changed.

    public string Name { get; }
    public long Age { get; }

    public List<string> Property { get; }
    readonly HashSet<int> field;

    // analyzer: complete-constructor
    public Sample(string name, int age)
    {
        Name = name;
        Age = age;

        // Because of assignments, these members are excluded from auto-generated code.
        Property = new List<string>();
        field = new HashSet<int>();
    }
}
