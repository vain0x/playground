using System;
using System.Collections.Generic;
using System.Text;
using VainZero.Collections;
using VainZero.Misc;

namespace VainZero.Parsing
{
    public sealed class Parser<TValue>
    {
        readonly Func<string, ParseResult<TValue>> parse;

        public ParseResult<TValue> Parse(string source)
        {
            return parse(source);
        }

        public Parser(Func<string, ParseResult<TValue>> parse)
        {
            this.parse = parse;
        }

        #region Operators
        public Parser<Y> Select<Y>(Func<TValue, Y> func)
        {
            return
                ParserModule.Create(source =>
                {
                    var result = Parse(source);
                    return
                        ParseResultModule.If(
                            result.IsOk,
                            () => (func(result.Value), result.Rest),
                            () => source
                        );
                });
        }

        public Parser<Y> SelectMany<Y>(Func<TValue, Parser<Y>> f)
        {
            return
                ParserModule.Create(source =>
                {
                    var firstResult = Parse(source);
                    if (firstResult.IsOk)
                    {
                        var secondParser = f(firstResult.Value);
                        return secondParser.Parse(firstResult.Rest);
                    }
                    return ParseResultModule.Error<Y>(source);
                });
        }

        public Parser<Z> SelectMany<Y, Z>(Func<TValue, Parser<Y>> f, Func<TValue, Y, Z> g)
        {
            return SelectMany(x => f(x).Select(y => g(x, y)));
        }

        public Parser<(TValue first, Y second)> Then<Y>(Parser<Y> secondParser)
        {
            return
                from first in this
                from second in secondParser
                select (first, second);
        }

        public Parser<TValue> Or(Parser<TValue> secondParser)
        {
            return
                ParserModule.Create(source =>
                {
                    var firstResult = Parse(source);
                    if (firstResult.IsOk) return firstResult;
                    var secondResult = secondParser.Parse(source);
                    if (secondResult.IsOk) return secondResult;
                    return ParseResultModule.Error<TValue>(source);
                });
        }

        public Parser<NonemptyList<TValue>> Many1
        {
            get
            {
                return
                    ParserModule.Create(source =>
                    {
                        var firstResult = Parse(source);
                        if (!firstResult.IsOk)
                        {
                            return ParseResultModule.Error<NonemptyList<TValue>>(source);
                        }

                        var tail = new List<TValue>();
                        var rest = firstResult.Rest;
                        while (true)
                        {
                            var result = Parse(rest);
                            if (!result.IsOk) break;

                            tail.Add(result.Value);
                            rest = result.Rest;
                        }

                        var values = NonemptyListModule.Create(firstResult.Value, tail);
                        return ParseResultModule.Ok(values, rest);
                    });
            }
        }

        public Parser<Unit> NotFollow
        {
            get
            {
                return
                    ParserModule.Create(source =>
                        ParseResultModule.If(
                            !Parse(source).IsOk,
                            () => (default(Unit), source),
                            () => source
                        ));
            }
        }
        #endregion
    }
}
