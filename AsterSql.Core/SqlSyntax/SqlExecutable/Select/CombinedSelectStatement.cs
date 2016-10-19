using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AsterSql.Core.SqlSyntax
{
    sealed class CombinedSelectStatement
    {
        public SelectStatement Statement { get; }
        public string Combinator { get; }

        public CombinedSelectStatement(SelectStatement statement, string combinator)
        {
            Statement = statement;
            Combinator = combinator;
        }
    }
}
