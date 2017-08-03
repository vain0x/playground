public class Person
    : PersonBase
{
    public string ReadOnlyAutoProperty { get; }

    public string ReadOnlyAutoPropertyWithInitializer { get; } = "default";

    public override string ReadOnlyAutoPropertyOverride { get; }

    public override string ReadOnlyAutoPropertyOverrideWithInitializer { get; } = "default";

    public string AutoPropertyWithInitializer { get; set; } = "default";
}

public abstract class PersonBase
{
    public abstract string ReadOnlyAutoPropertyOverride { get; }
    public abstract string ReadOnlyAutoPropertyOverrideWithInitializer { get; }
}
