using System;
using Optional;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Test
{
    public class Department
    {
        public ITable Table { get; }
        public IColumn<long> Id { get; }
        public IColumn<string> Name { get; }
        public IColumn<string> Email { get; }

        public Department(SqlBuilder sqlBuilder, Option<string> tableAlias)
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
    }
}
