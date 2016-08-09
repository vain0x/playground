using FluentSqlBuilder.Interface;

namespace FluentSqlBuilder.Detail
{
    /// <summary>
    /// データベース操作コマンドを表します。
    /// 具体的には select, insert, update, delete コマンドです。
    /// </summary>
    public abstract class ManipulationStatement
    {
        public JoinedRelation Source { get; private set; } =
            new JoinedRelation();
    }
}
