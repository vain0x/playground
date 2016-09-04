namespace FluentSqlBuilder.Public
{
    public interface ISqlTypeTag
    {
    }

    public interface IScalar
        : ISqlTypeTag
    {
    }

    public interface IScalar<out T>
        : IScalar
    {
    }

    public interface IRelation
        : ISqlTypeTag
    {
    }

    public interface IRelation<out T0>
        : IRelation
    {
    }
}
