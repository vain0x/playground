using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Xunit;
using AsterSql.Core;
using AsterSql.Core.SqlSyntax;
using AsterSql.TypedRecord;

namespace AsterSql.Test
{
    public class TableTest
    {
        static SqlBuilder Sql => FakeDb.Sql;
        static Table EmployeeTable => FakeDb.Employee.Table;

        [Fact]
        public void TestColumnProperties()
        {
            Relation.ColumnProperties(typeof(Employee))
                .Select(propertyInfo => propertyInfo.Name)
                .ToArray()
                .ShouldEqual(new[] { "Name", "Age", "DepartmentId" });
        }

        [Fact]
        public void TestInsert()
        {
            var employee = FakeDb.Employee;
            Sql.InsertValues(employee.Table, record =>
            {
                employee.Name[record] = "Miku";
                employee.Age[record] = 16L;
            })
                .ToEmbeddedString()
                .ShouldEqual(
                    "insert into `employees` (`name`,`age`,`department_id`)"
                    + " values ('Miku',16,null)");
        }
    }
}
