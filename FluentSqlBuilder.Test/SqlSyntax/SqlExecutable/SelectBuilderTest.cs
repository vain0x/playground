using Optional;
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
        public void TestUnion()
        {
            var employee = new Employee(Sql, "e".Some());
            Sql.Select()
                .From(employee.Table)
                .Where(employee.Name.Equal(Sql.String("Miku")))
                .FieldAll(employee.Table)
                .Union()
                .From(employee.Table)
                .Where(employee.Age.IsNull())
                .FieldAll(employee.Table)
                .ToRelation()
                .ToEmbeddedString()
                .ShouldEqual(
                    "( select `e`.* from `employees` as `e` where `e`.`name` = 'Miku'"
                    + " union"
                    + " select `e`.* from `employees` as `e` where `e`.`age` is null )"
                );
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
        public void TestInsertSelect()
        {
            var employee = FakeDb.Employee;
            Sql.Select()
                .From(employee.Table)
                .Insert(employee.Table, r =>
                {
                    employee.Name[r] = employee.Name.Concat(Sql.String("-san"));
                    employee.Age[r] = Sql.Int(17L);
                    employee.DepartmentId[r] = employee.DepartmentId;
                })
                .ToEmbeddedString()
                .ShouldEqual(
                    "insert into `employees` (`name`,`age`,`department_id`)"
                    + " select concat ( `employees`.`name` , '-san' )"
                    + " , 17"
                    + " , `employees`.`department_id`"
                    + " from `employees`"
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
