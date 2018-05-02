using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;

namespace VainZero.Parsing
{
    public static class ParserModule
    {
        /// <summary>
        /// Converts a function (delegate) to a parser.
        /// </summary>
        /// <typeparam name="X"></typeparam>
        /// <param name="parse"></param>
        /// <returns></returns>
        public static Parser<X> Create<X>(Func<string, ParseResult<X>> parse)
        {
            return new Parser<X>(parse);
        }

        /// <summary>
        /// Creates a parser which parses nothing and returns a successful result.
        /// </summary>
        /// <typeparam name="X"></typeparam>
        /// <param name="value"></param>
        /// <returns></returns>
        public static Parser<X> FromResult<X>(X value)
        {
            return Create(source => ParseResultModule.Ok(value, source));
        }

        /// <summary>
        /// Creates a parser which parses the specified string.
        /// </summary>
        /// <param name="expectedString"></param>
        /// <returns></returns>
        public static Parser<string> OfString(string expectedString)
        {
            return
                Create(source =>
                    ParseResultModule.If(
                        source.StartsWith(expectedString),
                        () => (expectedString, source.Substring(expectedString.Length)),
                        () => source
                    ));
        }

        /// <summary>
        /// Creates a parser which parses a character which satisfies the specified condition.
        /// </summary>
        /// <param name="expectedChars"></param>
        /// <returns></returns>
        public static Parser<char> Satisfy(Func<char, bool> satisfies)
        {
            return
                Create(source =>
                    ParseResultModule.If(
                        source.Length >= 1 && satisfies(source[0]),
                        () => (source[0], source.Substring(1)),
                        () => source
                    ));
        }
    }
}
