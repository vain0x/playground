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
using SQLite.CodeFirst;

namespace MicroStream.Data.Entity
{
    public class AppDbContext
        : DbContext
    {
        public AppDbContext()
            : base(new SQLiteConnection(ConnectionString), contextOwnsConnection: true)
        {
            Database.Log = text => Debug.WriteLine(text);
        }

        #region ConnectionString
        const string databaseFilePath = @"./data/database.sqlite";

        static SQLiteConnectionStringBuilder ConnectionStringBuilder =>
            new SQLiteConnectionStringBuilder
            {
                DataSource = databaseFilePath,
                ForeignKeys = true,
                BinaryGUID = false,
            };

        static string ConnectionString { get; } =
            ConnectionStringBuilder.ConnectionString;
        #endregion

        #region OnModelCreating
        class AppDbInitializer
            : SqliteDropCreateDatabaseWhenModelChanges<AppDbContext>
        {
            public AppDbInitializer(DbModelBuilder mb)
                : base(mb)
            {
                mb.Entity<Sequence>();
                mb.Entity<TwitterAuth>();
                mb.Entity<MastodonApp>();
                mb.Entity<MastodonAuth>();
            }
        }

        protected sealed override void OnModelCreating(DbModelBuilder mb)
        {
            Database.SetInitializer(new AppDbInitializer(mb));
        }
        #endregion
    }
}
