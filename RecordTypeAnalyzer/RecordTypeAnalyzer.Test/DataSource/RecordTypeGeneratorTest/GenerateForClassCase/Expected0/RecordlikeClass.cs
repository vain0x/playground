using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public sealed class RecordlikeClass
{
    /// <summary>
    /// Read-only property.
    /// Reference type.
    /// Lower camel case of the name is reserved.
    /// </summary>
    public string String { get; }

    /// <summary>
    /// Read-only property.
    /// Value type.
    /// Qualified type name.
    /// </summary>
    public System.TimeSpan TimeSpan { get; }

    public System.ComponentModel.Container PropertyWithPrivateSetter { get; private set; }

    /// <summary>
    /// Read-only field.
    /// The name is a contextural keyword.
    /// </summary>
    readonly System.Collections.Generic.IEnumerable<int> var;

    readonly int fieldWithInitializer = 1;

    public int PropertyWithInitializer { get; } = 1;

    public RecordlikeClass(string @string, System.TimeSpan timeSpan, System.ComponentModel.Container propertyWithPrivateSetter, System.Collections.Generic.IEnumerable<int> var)
    {
        if (@string == null)
            throw new System.ArgumentNullException(nameof(@string));
        if (propertyWithPrivateSetter == null)
            throw new System.ArgumentNullException(nameof(propertyWithPrivateSetter));
        if (var == null)
            throw new System.ArgumentNullException(nameof(var));
        String = @string;
        TimeSpan = timeSpan;
        PropertyWithPrivateSetter = propertyWithPrivateSetter;
        this.var = var;
    }
}
