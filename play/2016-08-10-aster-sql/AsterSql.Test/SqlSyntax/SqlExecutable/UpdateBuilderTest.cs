using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Xunit;
using AsterSql.SqlSyntax;

namespace AsterSql.Test
{
    public class UpdateBuilderTest
    {
        static SqlBuilder Sql => FakeDb.Sql;
        static Employee Employee => FakeDb.Employee;

        [Fact]
        public void TestUpdate()
        {
            Sql
                .Update(Employee.Table)
                .Set(r =>
                {
                    Employee.Name[r] = Employee.Name.Concat(Sql.String("!"));
                    Employee.Age[r] = Sql.Int(17);
                })
                .Where(Employee.Age.Equal(Sql.Int(20)))
                .ToCommand()
                .ToEmbeddedString()
                .ShouldEqual(
                    "update `employees`"
                    + " set `employees`.`name` = concat ( `employees`.`name` , '!' )"
                    + " , `employees`.`age` = 17"
                    + " where `employees`.`age` = 20"
                );
        }
    }
}
