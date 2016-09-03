using System;

namespace FluentSqlBuilder.Provider
{
    public abstract class SqlLanguage
    {
        /// <summary>
        /// 文字列がクオートも修飾もされていない識別子であることを検査します。
        /// </summary>
        public abstract bool IsRawIdentifier(string identifier);

        /// <summary>
        /// 名前をクオートした文字列を取得します。
        /// </summary>
        public abstract string QuoteIdentifier(string identifier);

        /// <summary>
        /// 修飾された文字列を取得します。
        /// </summary>
        public abstract string QualifyIdentifier(string qualifier, string columnName);

        /// <summary>
        /// ワイルドマーク * を修飾した形の文字列を取得します。
        /// </summary>
        public abstract string BuildWildmark(string tableAlias);

        /// <summary>
        /// テーブル名を表す文字列を取得します。
        /// </summary>
        public virtual string BuildTableName(string tableName)
        {
            return QuoteIdentifier(tableName);
        }

        /// <summary>
        /// カラム名を表す文字列を取得します。
        /// </summary>
        public virtual string BuildColumnName(string tableAlias, string columnName)
        {
            return QualifyIdentifier(tableAlias, columnName);
        }
    }
}
