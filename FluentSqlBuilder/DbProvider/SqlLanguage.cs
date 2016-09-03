using System;

namespace FluentSqlBuilder.Provider
{
    public abstract class SqlLanguage
    {
        /// <summary>
        /// 文字列が有効な識別子であることを検査します。
        /// </summary>
        public abstract bool IsIdentifier(string identifier);

        /// <summary>
        /// 名前をクオートした文字列を取得します。
        /// 事前保証: identifier は有効な識別子である。
        /// </summary>
        public abstract string QuoteIdentifier(string identifier);

        /// <summary>
        /// 名前を修飾した文字列を取得します。
        /// 事前保証: identifier は有効な識別子である。
        /// </summary>
        public abstract string QualifyIdentifier(string qualifier, string identifier);

        /// <summary>
        /// 名前を修飾およびクオートした文字列を取得します。
        /// </summary>
        public string BuildIdentifier(string qualifierOrNull, string identifier)
        {
            if (qualifierOrNull != null && !IsIdentifier(qualifierOrNull))
            {
                throw new ArgumentException(nameof(qualifierOrNull));
            }
            if (identifier == null || !IsIdentifier(identifier))
            {
                throw new ArgumentException(nameof(identifier));
            }

            var quoted = QuoteIdentifier(identifier);
            return
                qualifierOrNull == null
                    ? quoted
                    : QualifyIdentifier(QuoteIdentifier(qualifierOrNull), quoted);
        }

        /// <summary>
        /// ワイルドマーク * を修飾した形の文字列を取得します。
        /// </summary>
        public abstract string BuildWildmark(string qualifier);
    }
}
