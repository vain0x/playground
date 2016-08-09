namespace FluentSqlBuilder.Interface
{
    public interface IDbLanguage
    {
        /// <summary>
        /// 文字列がテーブル名として有効な識別子であることを検査します。
        /// </summary>
        bool IsTableName(string word);

        /// <summary>
        /// 文字列がカラム名として有効な識別子であることを検査します。
        /// </summary>
        bool IsColumnName(string word);

        /// <summary>
        /// 文字列が1つのリテラルを表すSQL式であることを検査します。
        /// </summary>
        bool IsLiteral(string expression);

        /// <summary>
        /// テーブル名を修飾した文字列を取得します。
        /// </summary>
        string QualifyTableName(string qualifier, string tableName);

        /// <summary>
        /// テーブル名をエスケープした文字列を取得します。
        /// </summary>
        string EscapeTableName(string tableName);

        /// <summary>
        /// カラム名を修飾した文字列を取得します。
        /// </summary>
        string QualifyColumnName(string qualifier, string columnName);

        /// <summary>
        /// カラム名をエスケープした文字列を取得します。
        /// </summary>
        string EscaleColumnName(string columnName);
    }
}
