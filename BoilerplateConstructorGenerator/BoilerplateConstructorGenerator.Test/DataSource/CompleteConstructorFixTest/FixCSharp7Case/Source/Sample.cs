using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public class Sample
{
    // The order is changed.

    public int Age { get; }
    public string Name { get; }

    // -*- complete-constructor -*-
    /// <summary>
    /// My complete constructor.
    /// </summary>
    public Sample(string name, int age)
    {
        Name = name ?? "";
        Age = age;
    }
}
