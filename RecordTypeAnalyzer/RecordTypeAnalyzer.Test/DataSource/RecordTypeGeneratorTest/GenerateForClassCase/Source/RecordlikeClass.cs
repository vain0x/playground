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
    readonly IEnumerable<int> var;

    readonly int fieldWithInitializer = 1;

    public int PropertyWithInitializer { get; } = 1;
}
