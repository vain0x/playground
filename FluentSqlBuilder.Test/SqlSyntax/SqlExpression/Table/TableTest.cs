using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FluentSqlBuilder.Detail;
using FluentSqlBuilder.Public;
using Xunit;
namespace FluentSqlBuilder.Test
{
    public class TableTest
    {
        static Table<Employee> EmployeeTable => (Table<Employee>)FakeDb.Employee.Table;

        [Fact]
        public void TestColumnProperties()
        {
            EmployeeTable
                .ColumnProperties()
                .Select(propertyInfo => propertyInfo.Name)
                .ToArray()
                .ShouldEqual(new[] { "Name", "Age", "DepartmentId" });
        }
    }
}
