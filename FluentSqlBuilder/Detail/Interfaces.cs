using System.Collections.Generic;
using System.Data.Common;

namespace FluentSqlBuilder.Detail
{
    /// <summary>
    /// SQL文の断片を表します。
    /// </summary>
    public interface ISqlPart
    {
        IEnumerable<string> Tokens { get; }
        IEnumerable<DbParameter> Parameters { get; }
    }

    /// <summary>
    /// リレーションを操作するコマンドを表します。
    /// 具体的には select クエリーか insert, update, delete コマンドです。
    /// </summary>
    public interface IRelationalQueryOrCommand
    {
        JoinedRelation Source { get; }
    }
}
