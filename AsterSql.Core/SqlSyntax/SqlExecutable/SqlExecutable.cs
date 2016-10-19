using System.Collections.Generic;
using System.Data.Common;

namespace AsterSql.SqlSyntax
{
    /// <summary>
    /// コマンドとして実行可能なSQL文であることを表す。
    /// </summary>
    public interface ISqlExecutable
    {
        DbCommand ToCommand();
    }
}
