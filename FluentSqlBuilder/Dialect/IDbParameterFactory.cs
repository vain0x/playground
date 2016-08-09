using System.Data;
using System.Data.Common;

namespace FluentSqlBuilder
{
    public interface IDbParameterFactory
    {
        DbParameter Create(string name, DbType type, object value);
    }
}
