using System;
using Optional;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Test
{
    public class Employee
    {
        public ITable Table { get; }
        public IColumn<string> Name { get; }
        public IColumn<long> Age { get; }
        public IColumn<long> DepartmentId { get; }

        public Employee(SqlBuilder sqlBuilder, Option<string> tableAlias)
        {
            Table = sqlBuilder.Table<Employee>("employees", tableAlias);
            Name = Table.Column<string>("name");
            Age = Table.Column<long>("age");
            DepartmentId = Table.Column<long>("department_id");
        }

        public Employee(SqlBuilder sqlBuilder)
            : this(sqlBuilder, Option.None<string>())
        {
        }
    }
}
