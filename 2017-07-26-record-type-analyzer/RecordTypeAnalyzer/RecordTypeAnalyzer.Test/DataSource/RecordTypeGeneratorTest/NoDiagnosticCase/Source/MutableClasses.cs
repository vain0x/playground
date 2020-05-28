using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public sealed class ClassWithMutableProperty
{
    public int MutableProperty { get; set; }

    public int ReadOnlyProperty { get; }
}

public sealed class ClassWithMutableField
{
    int mutableField;

    readonly int readonlyField;
}
