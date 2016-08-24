namespace FluentSqlBuilder
{
    public interface ISqlTypeTag
    {
    }

    public interface IScalar<out T>
        : ISqlTypeTag
    {
    }

    public interface IRelation
        : ISqlTypeTag
    {
    }
}
