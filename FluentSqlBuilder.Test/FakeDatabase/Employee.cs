using System;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Test
{
    public class Employee
    {
        public ITable Table { get; }
        public IColumn<string> Name { get; }
        public IColumn<long> Age { get; }
        public IColumn<long> DepartmentId { get; }

        public Employee(SqlBuilder sqlBuilder)
        {
            Table = sqlBuilder.Table("employees");
            Name = Table.Column<string>("name");
            Age = Table.Column<long>("age");
            DepartmentId = Table.Column<long>("department_id");
        }
    }
}
