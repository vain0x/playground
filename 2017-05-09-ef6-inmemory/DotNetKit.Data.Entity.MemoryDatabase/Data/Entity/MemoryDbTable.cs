using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using DotNetKit.Collections;

namespace DotNetKit.Data.Entity
{
    interface IMemoryDbTable
    {
        IEntityOperation EntityOperation { get; }
        IMemoryDbSet Connect();
    }

    abstract class MemoryDbTable<TKey, TEntity>
        : IMemoryDbTable
        where TEntity : class
    {
        public abstract MemoryDbSet<TEntity> Connect();

        public EntityOperation<TEntity> EntityOperation { get; }
        public IDictionary<TKey, TEntity> Relation { get; }

        #region IMemoryDbTable
        IEntityOperation IMemoryDbTable.EntityOperation => EntityOperation;
        IMemoryDbSet IMemoryDbTable.Connect() => Connect();
        #endregion

        protected MemoryDbTable(IEntityOperation entityOperation)
        {
            EntityOperation = (EntityOperation<TEntity>)entityOperation;
            Relation = new Dictionary<TKey, TEntity>(StructuralEqualityComparer<TKey>.Instance);
        }
    }

    sealed class NaturalKeyedMemoryDbTable<TEntity>
        : MemoryDbTable<object[], TEntity>
        where TEntity : class
    {
        public override MemoryDbSet<TEntity> Connect()
        {
            return new NaturalKeyedMemoryDbSet<TEntity>(this);
        }

        public NaturalKeyedMemoryDbTable(IEntityOperation entityOperation)
            : base(entityOperation)
        {
        }
    }

    sealed class SurrogateKeyedMemoryDbTable<TEntity>
        : MemoryDbTable<long, TEntity>
        where TEntity : class
    {
        long count = 0L;
        public long NextId()
        {
            return Interlocked.Increment(ref count);
        }

        public override MemoryDbSet<TEntity> Connect()
        {
            return new SurrogateKeyedMemoryDbSet<TEntity>(this);
        }

        public SurrogateKeyedMemoryDbTable(IEntityOperation entityOperation)
            : base(entityOperation)
        {
        }
    }

    static class MemoryDbTableModule
    {
        public static IMemoryDbTable Create(Type entityType)
        {
            var entityOperation = EntityOperationModule.Create(entityType);

            var tableType =
                entityOperation.IsSurrogateKeyed
                    ? typeof(SurrogateKeyedMemoryDbTable<>).MakeGenericType(entityType)
                    : typeof(NaturalKeyedMemoryDbTable<>).MakeGenericType(entityType);
            var constructor =
                tableType.GetConstructor(new[] { typeof(IEntityOperation) });
            return (IMemoryDbTable)constructor.Invoke(new[] { entityOperation });
        }
    }
}
