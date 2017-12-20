using System;
using System.Collections.Generic;
using System.Text;

namespace VainZero.Parsing
{
    public static class ParseResultModule
    {
        public static ParseResult<X> Ok<X>(X value, string rest)
        {
            return new ParseResult<X>(true, rest, value);
        }

        public static ParseResult<X> Error<X>(string rest)
        {
            return new ParseResult<X>(false, rest, default(X));
        }

        public static ParseResult<X>
            If<X>(
                bool condition,
                Func<(X value, string rest)> getOk,
                Func<string> getError
            )
        {
            if (condition)
            {
                (X value, string rest) = getOk();
                return Ok(value, rest);
            }
            else
            {
                return Error<X>(getError());
            }
        }
    }
}
