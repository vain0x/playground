﻿using System;
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
    /// Qualified type name.
    /// </summary>
    public System.TimeSpan TimeSpan { get; }

    /// <summary>
    /// Field.
    /// The name is a contextural keyword.
    /// </summary>
    readonly IEnumerable<int> async;

    #region Complete Constructor v1.0.0
    public Sample(string @string, System.TimeSpan timeSpan, System.Collections.Generic.IEnumerable<int> @async)
    {
        if (@string == null)
            throw new System.ArgumentNullException(nameof(@string));
        if (@async == null)
            throw new System.ArgumentNullException(nameof(@async));
        String = @string;
        TimeSpan = timeSpan;
        this.async = @async;
    }
    #endregion
}
