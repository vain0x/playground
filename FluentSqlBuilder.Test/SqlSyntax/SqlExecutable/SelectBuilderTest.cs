using Xunit;
using FluentSqlBuilder.Detail;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Test
{
    public class SelectBuilderTest
    {
        static SqlBuilder Sql => FakeDb.Sql;

        [Fact]
        public void MinimumTest()
        {
            var employee = FakeDb.Employee;
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
            var employee = FakeDb.Employee;
            var c = Sql.Select()
                .From(employee.Table)
                .Where(employee.Name.Equal(Sql.String("Miku")))
                .Field(employee.Age)
                .ToCommand();
            Assert.Equal(
                "select `employees`.`age` from `employees` where `employees`.`name` = @p0",
                c.ToParameterizedString()
            );
        }

        [Fact]
        public void TestFieldAll()
        {
            var employee = FakeDb.Employee;
            Sql.Select()
                .From(employee.Table)
                .FieldAll(employee.Table)
                .ToCommand()
                .ToParameterizedString()
                .ShouldEqual("select `employees`.* from `employees`");
        }

        [Fact]
        public void TestToScalar()
        {
            var employee = FakeDb.Employee;
            Sql.Select()
                .From(employee.Table)
                .Where(employee.Name.Equal(Sql.String("Miku")))
                .ToScalar(employee.Age)
                .ToEmbeddedString()
                .ShouldEqual(
                    "( select `employees`.`age` from `employees`"
                    + " where `employees`.`name` = 'Miku' )"
                );
        }

        [Fact]
        public void TestToRelation()
        {
            var employee = FakeDb.Employee;
            Sql.Select()
                .From(employee.Table)
                .Where(employee.Name.Equal(Sql.String("Miku")))
                .FieldAll(employee.Table)
                .ToRelation()
                .ToEmbeddedString()
                .ShouldEqual(
                    "( select `employees`.* from `employees`"
                    + " where `employees`.`name` = 'Miku' )"
                );
        }
    }
}
