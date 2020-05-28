using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Data.SQLite;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using SQLite.CodeFirst;

namespace VainZero.Dancehall.Data.Entity
{
    public sealed class AppDbContext
        : DbContext
    {
        public DbSet<Album> Albums { get; set; }
        public DbSet<AlbumItem> AlbumItems { get; set; }
        public DbSet<Person> Persons { get; set; }
        public DbSet<Playlist> Playlists { get; set; }
        public DbSet<PlaylistItem> PlaylistItems { get; set; }
        public DbSet<Sequence> Sequences { get; set; }
        public DbSet<Tag> Tags { get; set; }
        public DbSet<Work> Works { get; set; }
        public DbSet<WorkParty> WorkPartys { get; set; }
        public DbSet<WorkPartyType> WorkPartyTypes { get; set; }
        public DbSet<WorkPlay> WorkPlays { get; set; }
        public DbSet<WorkTagPair> WorkTagPairs { get; set; }

        protected override void OnModelCreating(DbModelBuilder modelBuilder)
        {
            var initializer = new SqliteCreateDatabaseIfNotExists<AppDbContext>(modelBuilder);
            Database.SetInitializer(initializer);
        }

        static SQLiteConnectionStringBuilder ConnectionStringBuilder { get; } =
            new SQLiteConnectionStringBuilder()
            {
                DataSource = @"data/database.db",
            };

        static string ConnectionString { get; } =
            ConnectionStringBuilder.ToString();

        public AppDbContext()
            : base(new SQLiteConnection(ConnectionString), contextOwnsConnection: true)
        {
        }
    }
}
