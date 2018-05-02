using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;

namespace VainZero.Parsing
{
    public static class ParserPresetModule
    {
        static bool IsAsciiDigit(char c)
        {
            return '0' <= c && c <= '9';
        }

        static bool IsAsciiAlphabet(char c)
        {
            return ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z');
        }

        public static Parser<char> DigitParser =>
            ParserModule.Satisfy(IsAsciiDigit);

        public static Parser<long> Int64Parser =
            DigitParser.Many1.Select(chars =>
            {
                var value = (long)(chars.First - '0');
                foreach (var c in chars.Rest)
                {
                    value = value * 10 + (c - '0');
                }
                return value;
            });

        public static Parser<char> IdentifierCharParser =>
            ParserModule.Satisfy(c => IsAsciiAlphabet(c) || IsAsciiDigit(c) || c == '_');

        public static Parser<string> IdentifierParser =>
            from _ in DigitParser.NotFollow
            from chars in IdentifierCharParser.Many1
            select new string(chars.ToArray());
    }
}
