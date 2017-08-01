public class Person
{
    public string Name { get; set; }

    /// <summary>
    /// Singleline case.
    /// </summary>
    public string NameWithSan => Name + "-san";

    /// <summary>
    /// Multiline case.
    /// </summary>
    public string NameOrDefault =>
        string.IsNullOrEmpty(Name)
            ? "John Doe"
            : Name;

    public int Age { get; set; }
}
