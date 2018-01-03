using System;
using System.Collections.Generic;
using System.Data.Common;
using System.Data.Entity;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.Sandbox
{
    public sealed class MyDbContext
        : DbContext
    {
        public MyDbContext(DbConnection connection)
            : base(connection, contextOwnsConnection: false)
        {
        }

        public DbSet<codes> codes { get; set; }
    }
}
