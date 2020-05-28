using System;
using System.Collections.Generic;
using System.Text;

namespace VainZero.Parsing
{
    /// <summary>
    /// Represents a result of parsing.
    /// </summary>
    /// <typeparam name="TValue"></typeparam>
    public struct ParseResult<TValue>
    {
        /// <summary>
        /// Gets a value indicating whether the result is successful.
        /// </summary>
        public bool IsOk { get; }

        public string Rest { get; }

        readonly TValue value;

        /// <summary>
        /// Gets the value if ok.
        /// Throws if error.
        /// </summary>
        public TValue Value =>
            IsOk
            ? value
            : throw new InvalidOperationException();

        public (string rest, TValue value) OkTuple =>
            (Rest, Value);

        internal ParseResult(bool isOk, string rest, TValue value)
        {
            IsOk = isOk;
            Rest = rest;
            this.value = value;
        }
    }
}
