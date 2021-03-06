﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Xunit;
using AsterSql.TypedRecord;
using AsterSql.SqlSyntax;

namespace AsterSql.Test
{
    public class ColumnTest
    {
        SqlBuilder Sql => FakeDb.Sql;

        [Fact]
        public void TestToColumn()
        {
            Sql.String("Miku").Concat(Sql.String("-san")).ToColumn("name")
                .ToEmbeddedString()
                .ShouldEqual("concat ( 'Miku' , '-san' ) as `name`");
        }
    }
}
