using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using FluentSqlBuilder.Interface;
using FluentSqlBuilder.Interface.Detail;

namespace FluentSqlBuilder.Implementation.Detail.SelectBuilder
{
    public class FromlessSelectBuilder
        : IFromlessSelectBuilder
    {
        public IOptionallyAliasedBuilder<IFieldlessSelectBuilder> From(string tableName)
        {

        }

        public IOptionallyAliasedBuilder<IFieldlessSelectBuilder> From(string qualifier, string tableName)
        {

        }
    }
}
