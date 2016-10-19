using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Xunit;
using AsterSql.Core;
using AsterSql.TypedRecord;
using AsterSql.Core.SqlSyntax;

namespace AsterSql.Test
{
    public class RelationTest
    {
        static SqlBuilder Sql => FakeDb.Sql;

        static Employee Employee => FakeDb.Employee;

        [Fact]
        public void TestFields_it_can_adds_fields_to_a_fieldless_select_builder()
        {
            var employee = Employee.As("e");
            Sql.Select()
                .From(employee.Table)
                .Fields(employee)
                .ToCommand()
                .ToEmbeddedString()
                .ShouldEqual(
                    "select `e`.`name` , `e`.`age` , `e`.`department_id`"
                    + " from `employees` as `e`"
                );
        }

        [Fact]
        public void TestFields_it_cannot_adds_zero_fields_to_a_fieldless_select_builder()
        {
            Assert.Throws<InvalidOperationException>(() =>
            {
                var columnless = new ColumnlessTable();
                var command = Sql.Select().From(Employee.Table).Fields(columnless).ToCommand();
            });
        }

        [Fact]
        public void TestFields_it_can_adds_fields_to_a_select_builder()
        {
            var employee = Employee.As("e");
            Sql.Select()
                .From(employee.Table)
                .Field(Sql.Int(1).As("one"))
                .Fields(employee)
                .ToCommand()
                .ToEmbeddedString()
                .ShouldEqual(
                    "select 1 as `one` , `e`.`name` , `e`.`age` , `e`.`department_id`"
                    + " from `employees` as `e`"
                );
        }

        [Fact]
        public void TestFields_it_can_add_zero_fields_to_a_select_builder()
        {
            var employees = Employee.As("e");
            Sql.Select()
                .From(Employee.Table)
                .Field(Sql.Int(1).As("one"))
                .Fields(new ColumnlessTable())
                .ToCommand()
                .ToEmbeddedString()
                .ShouldEqual("select 1 as `one` from `employees`");
        }
    }
}
