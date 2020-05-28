using System;
using DotNetKit.ErrorHandling;
using AsterSql.TypedRecord;

namespace AsterSql.Test
{
    public class Department
        : Relation
    {
        public Table Table { get; }
        public Column<long> Id { get; }
        public Column<string> Name { get; }
        public Column<string> Email { get; }

        Department(SqlBuilder sqlBuilder, Option<string> tableAlias)
        {
            Table = sqlBuilder.Table(this, "departments", tableAlias);
            Id = Table.Column<long>("department_id");
            Name = Table.Column<string>("department_name");
            Email = Table.Column<string>("department_email");
        }

        public Department(SqlBuilder sqlBuilder)
            : this(sqlBuilder, Option.None)
        {
        }

        public Department As(string alias)
        {
            return new Department(Table.SqlBuilder, Option.Some(alias));
        }
    }
}
