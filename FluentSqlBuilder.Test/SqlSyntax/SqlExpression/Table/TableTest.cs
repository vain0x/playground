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
        [Fact]
        public void TestColumnProperties()
        {
            var table = (Table<Employee>)FakeDb.Employee.Table;
            table
                .ColumnProperties()
                .Select(propertyInfo => propertyInfo.Name)
                .ToArray()
                .ShouldEqual(new[] { "Name", "Age", "DepartmentId" });
        }
    }
}
