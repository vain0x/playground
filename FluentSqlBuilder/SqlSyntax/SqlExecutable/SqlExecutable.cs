using System.Collections.Generic;
using System.Data.Common;

namespace FluentSqlBuilder.Detail
{
    /// <summary>
    /// コマンドとして実行可能なSQL文であることを表す。
    /// </summary>
    public interface ISqlExecutable
        : ISqlPart
    {
        DbCommand ToCommand();
    }

    /// <summary>
    /// リレーションを操作するコマンドを表す。
    /// 具体的には select クエリーか insert, update, delete コマンドのいずれか。
    /// </summary>
    public interface IRelationalQueryOrCommand
    {
        JoinedRelation Source { get; }
    }
}
