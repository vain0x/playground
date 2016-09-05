using System.Collections.Generic;
using System.Data.Common;

namespace FluentSqlBuilder.Detail
{
    /// <summary>
    /// コマンドとして実行可能なSQL文であることを表す。
    /// </summary>
    public interface ISqlExecutable
    {
        DbCommand ToCommand();
    }
}
