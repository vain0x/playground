﻿using System;
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

    // analyzer: complete-constructor
    /// <summary>
    /// My complete constructor.
    /// </summary>
    public Sample(int age, string name)
    {
        if (name == null)
            throw new ArgumentNullException(nameof(name));
        Age = age;
        Name = name;
    }
}
