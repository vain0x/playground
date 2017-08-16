using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public class EmptyClass
{
}

public struct EmptyStruct
{
}

public class ClassWithIgnoredMembers
{
    static int staticField;

    public static int StaticProperty { get; }

    readonly int fieldWithInitializer = 1;

    public int PropertyWithInitializer { get; } = 1;

    public event EventHandler Event;

    public object this[int index]
    {
        get { throw new NotImplementedException(); }
    }
}

public static class StaticClass
{
    public static int Property { get; }
}

public abstract class AbstractClass
{
    public int Property { get; }
}

public interface IInterface
{
    int Property { get; }
}
