using System;
using FluentSqlBuilder.Detail;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Test
{
    public class Person
    {
        public ITable Table { get; }
        public IColumn<string> Name { get; }
        public IColumn<long> Age { get; }

        public Person(SqlBuilder sqlBuilder)
        {
            Table = sqlBuilder.Table("persons");
            Name = Table.Column<string>("name");
            Age = Table.Column<long>("age");
        }
    }
}
