using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.EntityClassGenerator.Domain
{
    public sealed class DbSchema
    {
        public IReadOnlyCollection<DbTable> Tables { get; }

        public DbSchema(IReadOnlyCollection<DbTable> tables)
        {
            Tables = tables;
        }
    }
}
