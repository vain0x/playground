﻿using Xunit;
using FluentSqlBuilder.Provider.Fake;
using FluentSqlBuilder.Public;

namespace FluentSqlBuilder.Test
{
    public class SelectBuilderTest
    {
        SqlBuilder Sql { get; } =
            new SqlBuilder(new FakeDbProvider());
        
        [Fact]
        public void MinimumTest()
        {
            var person = new Person(Sql);
            var c = Sql.Select()
                .From(person.Table)
                .Field(person.Name)
                .ToCommand();
            Assert.Equal("select `persons`.`name` from `persons`", c.CommandText);
            Assert.True(c.Parameters.Count == 0);
        }

        [Fact]
        public void WhereTest()
        {
            var person = new Person(Sql);
            var c = Sql.Select()
                .From(person.Table)
                .Where().Equal(person.Name, Sql.String("Miku"))
                .Field(person.Age)
                .ToCommand();
            Assert.Equal(
                "select `persons`.`age` from `persons` where ( `persons`.`name` = @p0 )",
                c.ToParameterizedString()
            );
        }
    }
}
