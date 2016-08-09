using System;

namespace FluentSqlBuilder.Implementation.Detail.SelectBuilder
{
    public class SelectStatement
    {
        public OptionallyAliased<string>[] TableName { get; set; }
    }
}
