using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Data.Entity;
using System.Data.Entity.Infrastructure;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using DotNetKit.Collections;
using DotNetKit.Data.Entity.Infrastructure;
using DotNetKit.Linq;

namespace DotNetKit.Data.Entity
{
    interface IMemoryDbSet
    {
        int SaveChanges();
    }

    abstract class MemoryDbSet<TEntity>
        : DbSet<TEntity>
        , IMemoryDbSet
        , IQueryable<TEntity>
        , IDbAsyncEnumerable<TEntity>
        where TEntity : class
    {
        public abstract int SaveChanges();

        public abstract IEnumerator<TEntity> GetEnumerator();

        protected abstract EntityOperation<TEntity> EntityOperation { get; }

        public override sealed ObservableCollection<TEntity> Local { get; } =
            new ObservableCollection<TEntity>();

        public override sealed TEntity Create()
        {
            return EntityOperation.Create();
        }

        public override sealed TDerived Create<TDerived>()
        {
            return Activator.CreateInstance<TDerived>();
        }

        public override sealed Task<TEntity>
            FindAsync(params object[] keyValues)
        {
            return Task.FromResult(Find(keyValues));
        }

        public override sealed Task<TEntity>
            FindAsync(CancellationToken cancellationToken, params object[] keyValues)
        {
            return FindAsync(keyValues);
        }

        public override sealed IEnumerable<TEntity> AddRange(IEnumerable<TEntity> entities)
        {
            var list = new List<TEntity>();
            foreach (var entity in entities)
            {
                Add(entity);
                list.Add(entity);
            }
            return list;
        }

        public override sealed IEnumerable<TEntity> RemoveRange(IEnumerable<TEntity> entities)
        {
            var list = new List<TEntity>();
            foreach (var entity in entities)
            {
                Remove(entity);
                list.Add(entity);
            }
            return list;
        }

        IQueryable<TEntity> AsQueryable { get; }

        #region IQueryable
        Type IQueryable.ElementType =>
            AsQueryable.ElementType;

        Expression IQueryable.Expression =>
            AsQueryable.Expression;

        IQueryProvider IQueryable.Provider =>
            AsQueryable.Provider;
        #endregion

        #region IDbAsyncEnumerable
        public IDbAsyncEnumerator<TEntity> GetAsyncEnumerator()
        {
            return new SynchronousDbAsyncEnumerator<TEntity>(GetEnumerator());
        }

        IDbAsyncEnumerator IDbAsyncEnumerable.GetAsyncEnumerator()
        {
            return GetAsyncEnumerator();
        }
        #endregion

        protected static void ThrowAttachingMoreThanOneEntityException()
        {
            throw new InvalidOperationException("Up to one entity can be attached for each key.");
        }

        protected static void ThrowRemovingNonexistingKeyException()
        {
            throw new InvalidOperationException("The entity to be removed not found.");
        }

        protected MemoryDbSet()
        {
            var enumerable = new AnonymousEnumerable<TEntity>(GetEnumerator);
            AsQueryable = new SynchronousAsyncQueryable<TEntity>(enumerable.AsQueryable());
        }
    }

    sealed class NaturalKeyedMemoryDbSet<TEntity>
        : MemoryDbSet<TEntity>
        where TEntity : class
    {
        NaturalKeyedMemoryDbTable<TEntity> Table { get; }

        protected override EntityOperation<TEntity> EntityOperation =>
            Table.EntityOperation;

        #region Changes
        enum ChangeType
        {
            Add,
            Remove,
            Update,
        }

        sealed class Change
        {
            public TEntity Entity { get; private set; }
            public object[] Key { get; private set; }
            public object[] Original { get; private set; }
            public ChangeType ChangeType { get; set; }

            public Change(TEntity entity, object[] key, object[] original, ChangeType changeType)
            {
                Entity = entity;
                Key = key;
                Original = original;
                ChangeType = changeType;
            }
        }

        Dictionary<object[], Change> Changes { get; } =
            new Dictionary<object[], Change>(StructuralEqualityComparer<object[]>.Instance);

        /// <summary>
        /// Tries to attach an entity to be tracked.
        /// Returns <c>true</c> if actually attached or <c>false</c> if already attached.
        /// </summary>
        bool TryAttach(TEntity entity, ChangeType changeType, out Change change)
        {
            var key = EntityOperation.KeyValues(entity);

            if (Changes.TryGetValue(key, out change))
            {
                if (entity != change.Entity) ThrowAttachingMoreThanOneEntityException();
                return false;
            }

            var nonkeyValues = EntityOperation.NonkeyValues(entity);
            change = new Change(entity, key, nonkeyValues, changeType);
            Changes.Add(key, change);
            Local.Add(entity);
            return true;
        }

        bool TryDetach(TEntity entity, out Change change)
        {
            var key = EntityOperation.KeyValues(entity);

            if (!Changes.TryGetValue(key, out change)) return false;
            if (change.Entity != entity) return false;

            Changes.Remove(key);
            Local.Remove(entity);
            return true;
        }
        #endregion

        public override TEntity Find(params object[] keyValues)
        {
            var change = default(Change);
            if (Changes.TryGetValue(keyValues, out change))
            {
                return change.ChangeType == ChangeType.Remove ? null : change.Entity;
            }

            var source = default(TEntity);
            if (!Table.Relation.TryGetValue(keyValues, out source))
            {
                return null;
            }

            var entity = EntityOperation.Clone(source);
            Attach(entity);
            return entity;
        }

        public override TEntity Attach(TEntity entity)
        {
            var track = default(Change);
            TryAttach(entity, ChangeType.Update, out track);
            return entity;
        }

        public override TEntity Add(TEntity entity)
        {
            var track = default(Change);
            if (!TryAttach(entity, ChangeType.Add, out track))
            {
                track.ChangeType =
                    (track.ChangeType == ChangeType.Remove)
                        ? ChangeType.Update
                        : ChangeType.Add;
            }
            return entity;
        }

        public override TEntity Remove(TEntity entity)
        {
            var change = default(Change);
            if (!TryAttach(entity, ChangeType.Remove, out change))
            {
                if (change.ChangeType == ChangeType.Add)
                {
                    TryDetach(entity, out change);
                }
                else
                {
                    change.ChangeType = ChangeType.Remove;
                }
            }
            return entity;
        }

        public override int SaveChanges()
        {
            var count = 0;

            var relation = Table.Relation;
            var changes = Changes.Values.ToArray();
            Changes.Clear();
            Local.Clear();

            foreach (var change in changes)
            {
                switch (change.ChangeType)
                {
                    case ChangeType.Add:
                        // Throws if the key conflicts.
                        relation.Add(change.Key, change.Entity);
                        count++;
                        break;
                    case ChangeType.Remove:
                        if (!relation.Remove(change.Key)) ThrowRemovingNonexistingKeyException();
                        count++;
                        break;
                    case ChangeType.Update:
                        var values = EntityOperation.NonkeyValues(change.Entity);
                        if (!values.SequenceEqual(change.Original))
                        {
                            // Throws if the key not found.
                            relation[change.Key] = change.Entity;
                            count++;
                        }
                        break;
                    default:
                        throw new Exception("changeType = " + change.ChangeType);
                }
            }

            return count;
        }

        IEnumerable<TEntity> Enumerate()
        {
            var keys =
                Changes.Select(ch => ch.Key)
                .Concat(Table.Relation.Keys)
                .Distinct(StructuralEqualityComparer<object[]>.Instance)
                .ToArray();

            foreach (var key in keys)
            {
                var entity = Find(key);
                if (entity != null)
                {
                    yield return entity;
                }
            }
        }

        public override IEnumerator<TEntity> GetEnumerator()
        {
            return Enumerate().GetEnumerator();
        }

        public NaturalKeyedMemoryDbSet(NaturalKeyedMemoryDbTable<TEntity> table)
        {
            Table = table;
        }
    }

    sealed class SurrogateKeyedMemoryDbSet<TEntity>
        : MemoryDbSet<TEntity>
        where TEntity : class
    {
        SurrogateKeyedMemoryDbTable<TEntity> Table { get; }

        protected override EntityOperation<TEntity> EntityOperation =>
            Table.EntityOperation;

        enum ChangeType
        {
            Add,
            Remove,
            Update,
        }

        sealed class Change
        {
            public TEntity Entity { get; private set; }
            public long Id { get; private set; }
            public object[] Original { get; private set; }
            public ChangeType ChangeType { get; set; }

            public Change(TEntity entity, long id, object[] original, ChangeType changeType)
            {
                Entity = entity;
                Id = id;
                Original = original;
                ChangeType = changeType;
            }
        }

        HashSet<TEntity> AddedEntities { get; } =
            new HashSet<TEntity>();

        Dictionary<long, Change> Changes { get; } =
            new Dictionary<long, Change>();

        public override TEntity Find(params object[] keyValues)
        {
            if (keyValues.Length != 1) throw new ArgumentException(nameof(keyValues));
            var id = (long)keyValues[0];

            var change = default(Change);
            if (Changes.TryGetValue(id, out change))
            {
                return change.ChangeType == ChangeType.Remove ? null : change.Entity;
            }

            var source = default(TEntity);
            if (!Table.Relation.TryGetValue(id, out source))
            {
                return null;
            }

            var entity = EntityOperation.Clone(source);
            Attach(entity);
            return entity;
        }

        public override TEntity Attach(TEntity entity)
        {
            var id = EntityOperation.GetSurrogateKey(entity);
            if (!EntityOperation<TEntity>.IsValidSurrogateKey(id))
            {
                throw new ArgumentException("Entity doesn't have valid ID.");
            }

            var change = default(Change);
            if (Changes.TryGetValue(id, out change))
            {
                if (change.Entity != entity) ThrowAttachingMoreThanOneEntityException();
                change.ChangeType = ChangeType.Update;
            }
            else
            {
                var nonkeyValues = EntityOperation.NonkeyValues(entity);
                change = new Change(entity, id, nonkeyValues, ChangeType.Update);
                Changes.Add(id, change);
            }
            return entity;
        }

        public override TEntity Add(TEntity entity)
        {
            var id = EntityOperation.GetSurrogateKey(entity);
            if (EntityOperation<TEntity>.IsValidSurrogateKey(id))
            {
                var change = default(Change);
                if (Changes.TryGetValue(id, out change))
                {
                    if (change.Entity != entity) ThrowAttachingMoreThanOneEntityException();
                }
                else
                {
                    var nonkeyValues = EntityOperation.NonkeyValues(entity);
                    change = new Change(entity, id, nonkeyValues, ChangeType.Add);
                    Changes.Add(id, change);
                }
            }
            else
            {
                AddedEntities.Add(entity);
            }
            return entity;
        }

        public override TEntity Remove(TEntity entity)
        {
            var id = EntityOperation.GetSurrogateKey(entity);
            if (EntityOperation<TEntity>.IsValidSurrogateKey(id))
            {
                var change = default(Change);
                if (Changes.TryGetValue(id, out change))
                {
                    if (change.Entity != entity) ThrowAttachingMoreThanOneEntityException();
                    change.ChangeType = ChangeType.Remove;
                }
                else
                {
                    var nonkeyValues = EntityOperation.NonkeyValues(entity);
                    change = new Change(entity, id, nonkeyValues, ChangeType.Remove);
                    Changes.Add(id, change);
                }
            }
            else
            {
                AddedEntities.Remove(entity);
            }
            return entity;
        }

        void AddToRelation(TEntity entity)
        {
            var id = EntityOperation.GetOrSetSurrogateKey(entity, () => Table.NextId());
            // Throws if the key conflicts.
            Table.Relation.Add(id, entity);
        }

        public override int SaveChanges()
        {
            var count = 0;

            var relation = Table.Relation;
            var addedEntities = AddedEntities.ToArray();
            var changes = Changes.Values.ToArray();
            AddedEntities.Clear();
            Changes.Clear();
            Local.Clear();

            foreach (var entity in addedEntities)
            {
                AddToRelation(entity);
                count++;
            }

            foreach (var change in changes)
            {
                switch (change.ChangeType)
                {
                    case ChangeType.Add:
                        AddToRelation(change.Entity);
                        count++;
                        break;
                    case ChangeType.Remove:
                        if (!relation.Remove(change.Id)) ThrowRemovingNonexistingKeyException();
                        count++;
                        break;
                    case ChangeType.Update:
                        var values = EntityOperation.NonkeyValues(change.Entity);
                        if (!values.SequenceEqual(change.Original))
                        {
                            // Throws if the key not found.
                            relation[change.Id] = change.Entity;
                            count++;
                        }
                        break;
                    default:
                        throw new Exception("changeType = " + change.ChangeType);
                }
            }

            return count;
        }

        IEnumerable<TEntity> Enumerate()
        {
            foreach (var entity in AddedEntities.ToArray())
            {
                yield return entity;
            }

            var ids =
                Changes.Values.Select(ch => ch.Id)
                .Concat(Table.Relation.Keys)
                .ToArray()
                .Distinct();

            foreach (var id in ids)
            {
                var entity = Find(id);
                if (entity != null)
                {
                    yield return entity;
                }
            }
        }

        public override IEnumerator<TEntity> GetEnumerator()
        {
            return Enumerate().GetEnumerator();
        }

        public SurrogateKeyedMemoryDbSet(SurrogateKeyedMemoryDbTable<TEntity> table)
        {
            Table = table;
        }
    }
}
