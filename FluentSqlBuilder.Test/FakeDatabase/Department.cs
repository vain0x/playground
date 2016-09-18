using System;
using Optional;
using FluentSqlBuilder.Accessor;

namespace FluentSqlBuilder.Test
{
    public class Department
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
            : this(sqlBuilder, Option.None<string>())
        {
        }

        public Department As(string alias)
        {
            return new Department(Table.SqlBuilder, alias.Some());
        }
    }
}
