using System;
using System.Collections.Generic;
using AsterSql.SqlSyntax;

namespace AsterSql.Data
{
    public abstract class SqlLanguage
    {
        /// <summary>
        /// 文字列がクオートも修飾もされていない識別子であることを検査する。
        /// </summary>
        public abstract bool IsRawIdentifier(string identifier);

        /// <summary>
        /// 名前をクオートした文字列を取得する。
        /// </summary>
        public abstract string QuoteIdentifier(string identifier);

        /// <summary>
        /// 修飾された文字列を取得する。
        /// </summary>
        public abstract string QualifyIdentifier(string qualifier, string columnName);

        /// <summary>
        /// ワイルドマーク * を取得する。
        /// </summary>
        public virtual string GetWildmark()
        {
            return "*";
        }

        /// <summary>
        /// ワイルドマーク * を修飾した形の文字列を組み立てる。
        /// </summary>
        public abstract string BuildWildmark(string tableAlias);

        /// <summary>
        /// テーブル名を表す文字列を組み立てる。
        /// </summary>
        public virtual string BuildTableName(string tableName)
        {
            return QuoteIdentifier(tableName);
        }

        /// <summary>
        /// カラム名を表す文字列を組み立てる。
        /// </summary>
        public virtual string BuildColumnName(string tableAlias, string columnName)
        {
            return QualifyIdentifier(tableAlias, columnName);
        }

        /// <summary>
        /// 識別子を連結する。
        /// </summary>
        public virtual string ConcatIdentifiers(string first, string second)
        {
            return $"{first}__{second}";
        }

        /// <summary>
        /// 別名つきの式の字句列を構築する。
        /// </summary>
        public virtual IEnumerable<SqlToken> ConstructAliasedExpression(
            IEnumerable<SqlToken> tokens,
            string alias
        )
        {
            foreach (var token in tokens) yield return token;
            yield return SqlToken.FromString("as");
            yield return SqlToken.FromString(QuoteIdentifier(alias));
        }
    }
}
