using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public class Sample
{
    public string Name { get; }

    // The type is changed.
    public long Age { get; }

    /// <summary>
    /// My complete constructor.
    /// </summary>
    // analyzer: complete-constructor
    public Sample(string name, long age)
    {
        if (name == null)
            throw new ArgumentNullException(nameof(name));
        Name = name;
        Age = age;
        Debug.WriteLine("Beginning person initialization.");

            // Do something.

        Debug.WriteLine("Ending person initialization.");
    }
}
