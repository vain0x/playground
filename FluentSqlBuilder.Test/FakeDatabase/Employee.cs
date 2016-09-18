﻿using System;
using Optional;
using FluentSqlBuilder.Accessor;

namespace FluentSqlBuilder.Test
{
    public class Employee
        : Relation
    {
        public Table Table { get; }
        public Column<string> Name { get; }
        public Column<long> Age { get; }
        public Column<long> DepartmentId { get; }

        Employee(SqlBuilder sqlBuilder, Option<string> tableAlias)
        {
            Table = sqlBuilder.Table(this, "employees", tableAlias);
            Name = Table.Column<string>("name");
            Age = Table.Column<long>("age");
            DepartmentId = Table.Column<long>("department_id");
        }

        public Employee(SqlBuilder sqlBuilder)
            : this(sqlBuilder, Option.None<string>())
        {
        }

        public Employee As(string alias)
        {
            return new Employee(Table.SqlBuilder, alias.Some());
        }
    }
}
