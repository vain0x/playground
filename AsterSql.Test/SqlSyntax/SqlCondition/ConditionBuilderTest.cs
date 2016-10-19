using Xunit;
using FluentSqlBuilder.SqlSyntax;

namespace FluentSqlBuilder.Test
{
    public class ConditionBuilderTest
    {
        static SqlBuilder Sql => FakeDb.Sql;

        #region Test: And
        [Fact]
        public void TestAnd_trivial()
        {
            Sql.And()
                .ToEmbeddedString()
                .ShouldEqual(SqlConditionConstant.TrueExpression);
        }

        [Fact]
        public void TestAnd_single_condition()
        {
            Sql.And()
                .And(FakeDb.Employee.Name.Equal(Sql.String("Miku")))
                .ToEmbeddedString()
                .ShouldEqual("`employees`.`name` = 'Miku'");
        }

        [Fact]
        public void TestAnd_three_conditions()
        {
            Sql.And()
                .And(FakeDb.Employee.Name.Equal(Sql.String("Miku")))
                .And(FakeDb.Employee.Age.Equal(Sql.Int(16L)))
                .And(FakeDb.Employee.DepartmentId.Equal(FakeDb.Department.Id))
                .ToEmbeddedString()
                .ShouldEqual(
                    "( `employees`.`name` = 'Miku' "
                    + "and `employees`.`age` = 16 "
                    + "and `employees`.`department_id` = `departments`.`department_id` )");
        }

        [Fact]
        public void TestAnd_collapse_true()
        {
            Sql.And()
                .And(FakeDb.Employee.Name.Equal(Sql.String("Miku")))
                .And(Sql.True)
                .And(Sql.True)
                .ToEmbeddedString()
                .ShouldEqual("`employees`.`name` = 'Miku'");
        }

        [Fact]
        public void TestAnd_combine_or_condition()
        {
            Sql.And()
                .And(FakeDb.Employee.Name.Equal(Sql.String("Miku")))
                .And(Sql.Null<long>().IsNull().Or(Sql.Int(1L).IsNull()))
                .ToEmbeddedString()
                .ShouldEqual("( `employees`.`name` = 'Miku' and ( null is null or 1 is null ) )");
        }
        #endregion
    }
}
