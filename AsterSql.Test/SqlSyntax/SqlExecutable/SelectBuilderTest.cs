using System;
using Optional;
using Xunit;
using AsterSql.Core;
using AsterSql.SqlSyntax;

namespace AsterSql.Test
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
        public void TestWhere_use_parameter_twice()
        {
            var employee = FakeDb.Employee;
            var zero = Sql.Int(0);
            Sql.Select()
                .From(employee.Table)
                .Where(zero.Equal(zero))
                .FieldAll(employee.Table)
                .ToCommand()
                .ToEmbeddedString()
                .ShouldEqual("select `employees`.* from `employees` where 0 = 0");
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
        public void TestJoinUsing()
        {
            var employee = FakeDb.Employee;
            var department = FakeDb.Department;
            Sql.Select()
                .From(employee.Table)
                .Join(department.Table).Using(employee.DepartmentId)
                .FieldAll(employee.Table)
                .FieldAll(department.Table)
                .ToCommand()
                .ToEmbeddedString()
                .ShouldEqual(
                    "select `employees`.* , `departments`.* from `employees`"
                    + " join `departments` using ( `department_id` )"
                );
        }

        [Fact]
        public void TestJoinOn()
        {
            var employee = FakeDb.Employee;
            var department = FakeDb.Department;
            Sql.Select()
                .From(employee.Table)
                .Join(department.Table).On(employee.DepartmentId.Equal(department.Id))
                .FieldAll(employee.Table)
                .FieldAll(department.Table)
                .ToCommand()
                .ToEmbeddedString()
                .ShouldEqual(
                    "select `employees`.* , `departments`.*"
                    + " from `employees` join `departments`"
                    + " on `employees`.`department_id` = `departments`.`department_id`"
                );
        }

        [Fact]
        public void TestJoin_self()
        {
            var e0 = new Employee(Sql).As("e0");
            var e1 = new Employee(Sql).As("e1");
            var e2 = new Employee(Sql).As("e2");
            Sql
                .Select()
                .From(e0.Table)
                .Join(e1.Table).On(e0.Name.Equal(e1.Name))
                .Join(e2.Table).On(e1.Age.Equal(e2.Age))
                .Field(e2.DepartmentId)
                .ToCommand()
                .ToEmbeddedString()
                .ShouldEqual(
                    "select `e2`.`department_id` from `employees` as `e0`"
                    + " join `employees` as `e1` on `e0`.`name` = `e1`.`name`"
                    + " join `employees` as `e2` on `e1`.`age` = `e2`.`age`"
                );
        }

        [Fact]
        public void TestUnion()
        {
            var employee = new Employee(Sql).As("e");
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
        public void TestAny()
        {
            var employee = FakeDb.Employee;
            Sql.Select()
               .From(employee.Table)
               .Where(employee.Age.Equal(Sql.Int(16L)))
               .Any(employee.Name)
               .ToEmbeddedString()
               .ShouldEqual(
                   "any ( select `employees`.`name` from `employees`"
                   + " where `employees`.`age` = 16 )"
                  );
        }

        [Fact]
        public void TestExists()
        {
            var employee = FakeDb.Employee;
            Sql.Select()
                .From(employee.Table)
                .Where(employee.Name.Equal(Sql.String("Miku")))
                .Exists()
                .ToEmbeddedString()
                .ShouldEqual(
                   "exists ( select * from `employees`"
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
        public void TestInsertSelect_missing()
        {
            var department = FakeDb.Department;
            Assert.ThrowsAny<Exception>(() =>
            {
                Sql.Select()
                    .From(department.Table)
                    .Insert(department.Table, r =>
                    {
                        department.Name[r] = Sql.String("Personal");
                        department.Email[r] = Sql.String("personal@example.com");
                    })
                    .ToEmbeddedString();
            });
        }

        [Fact]
        public void TestInsertSelect_null()
        {
            var department = new Department(Sql).As("d");
            Sql.Select()
                .From(department.Table)
                .Insert(department.Table, r =>
                {
                    department.Id[r] = Sql.Null<long>();
                    department.Name[r] = department.Name;
                    department.Email[r] = Sql.String("department at example.com");
                })
                .ToEmbeddedString()
                .ShouldEqual(
                    "insert into `departments` (`department_id`,`department_name`,`department_email`)"
                    + " select null , `d`.`department_name` , 'department at example.com'"
                    + " from `departments` as `d`"
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
