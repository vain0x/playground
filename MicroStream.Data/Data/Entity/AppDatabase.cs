using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Collections.Generic;
using System.Data.Entity;
using System.Data.Entity.ModelConfiguration.Conventions;
using System.Data.SQLite;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MicroStream.Data.Entity
{
    public class AppDatabase
        : IDatabase
    {
        public DbContext Connect()
        {
            return new AppDbContext();
        }
    }
}
