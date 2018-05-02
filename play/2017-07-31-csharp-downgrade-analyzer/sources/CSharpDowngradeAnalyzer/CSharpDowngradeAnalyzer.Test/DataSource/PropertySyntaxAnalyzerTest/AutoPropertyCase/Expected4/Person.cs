public class Person
    : PersonBase
{
    public string ReadOnlyAutoProperty { get; }

    public string ReadOnlyAutoPropertyWithInitializer { get; } = "default";

    public override string ReadOnlyAutoPropertyOverride { get; }

    public override string ReadOnlyAutoPropertyOverrideWithInitializer { get; } = "default";
    string autoPropertyWithInitializer = "default";

    public string AutoPropertyWithInitializer
    {
        get
        {
            return autoPropertyWithInitializer;
        }

        set
        {
            autoPropertyWithInitializer = value;
        }
    }
}

public abstract class PersonBase
{
    public abstract string ReadOnlyAutoPropertyOverride { get; }
    public abstract string ReadOnlyAutoPropertyOverrideWithInitializer { get; }
}
