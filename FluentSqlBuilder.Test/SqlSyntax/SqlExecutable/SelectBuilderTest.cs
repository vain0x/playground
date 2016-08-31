using Xunit;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Test
{
    public class SelectBuilderTest
    {
        static SqlBuilder Sql => FakeDb.Sql;

        [Fact]
        public void MinimumTest()
        {
            var employee = new Employee(Sql);
            var c = Sql.Select()
                .From(employee.Table)
                .Field(employee.Name)
                .ToCommand();
            Assert.Equal("select `employees`.`name` from `employees`", c.CommandText);
            Assert.True(c.Parameters.Count == 0);
        }

        [Fact]
        public void WhereTest()
        {
            var employee = new Employee(Sql);
            var c = Sql.Select()
                .From(employee.Table)
                .Where(employee.Name.Equal(Sql.String("Miku")))
                .Field(employee.Age)
                .ToCommand();
            Assert.Equal(
                "select `employees`.`age` from `employees` where ( `employees`.`name` = @p0 )",
                c.ToParameterizedString()
            );
        }
    }
}
