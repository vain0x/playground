using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

public class Sample
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
    /// Type name is redundantly qualified.
    /// </summary>
    public System.TimeSpan TimeSpan { get; }

    /// <summary>
    /// Field.
    /// The name is a contextural keyword.
    /// The type name is necessarily qualified.
    /// </summary>
    readonly System.Collections.IEnumerable async;

    // analyzer: complete-constructor
    public Sample(string @string, TimeSpan timeSpan, System.Collections.IEnumerable @async)
    {
        if (@string == null)
            throw new ArgumentNullException(nameof(@string));
        if (@async == null)
            throw new ArgumentNullException(nameof(@async));
        String = @string;
        TimeSpan = timeSpan;
        this.async = @async;
    }
}
