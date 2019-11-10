using System;
using System.Collections.Generic;
using System.Data.Entity;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace DotNetKit.Data.Entity
{
    sealed class MemoryDbContext
        : DbContext
    {
        public MemoryDbSchema Schema { get; }

        Dictionary<Type, IMemoryDbSet> Sets =
            new Dictionary<Type, IMemoryDbSet>();

        public override DbSet Set(Type entityType)
        {
            throw new NotSupportedException("Use Set<TEntity> instead.");
        }

        public override DbSet<TEntity> Set<TEntity>()
        {
            var set = default(IMemoryDbSet);
            if (!Sets.TryGetValue(typeof(TEntity), out set))
            {
                var table = Schema.GetOrAddTable<TEntity>();
                set = table.Connect();
                Sets.Add(typeof(TEntity), set);
            }
            return (DbSet<TEntity>)set;
        }

        public override int SaveChanges()
        {
            var sets = Sets.Values.ToArray();
            Sets.Clear();

            var count = 0;
            foreach (var set in sets)
            {
                count += set.SaveChanges();
            }
            return count;
        }

        public override Task<int> SaveChangesAsync()
        {
            return Task.FromResult(SaveChanges());
        }

        public override Task<int> SaveChangesAsync(CancellationToken cancellationToken)
        {
            return SaveChangesAsync();
        }

        protected override void Dispose(bool disposing)
        {
            Sets.Clear();
            base.Dispose(disposing);
        }

        public MemoryDbContext(MemoryDbSchema schema)
            : base()
        {
            Schema = schema;
        }
    }
}
