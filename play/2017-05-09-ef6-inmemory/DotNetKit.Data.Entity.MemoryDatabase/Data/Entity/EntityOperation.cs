using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Data.Entity;
using System.Linq;
using System.Text;
using System.Reflection;
using System.Threading.Tasks;

namespace DotNetKit.Data.Entity
{
    interface IEntityOperation
    {
        Type EntityType { get; }

        object Create();
        object Clone(object entity);
        object[] KeyValues(object entity);
        object[] NonkeyValues(object entity);
        bool IsSurrogateKeyed { get; }
    }

    sealed class EntityOperation<TEntity>
        : IEntityOperation
        where TEntity : class
    {
        public Type EntityType => typeof(TEntity);

        PropertyInfo[] KeyProperties { get; } =
            typeof(TEntity)
            .GetProperties()
            .Select(pi =>
                new {
                    PropertyInfo = pi,
                    KeyAttribute = pi.GetCustomAttribute<KeyAttribute>(),
                    ColumnAttribute = pi.GetCustomAttribute<ColumnAttribute>(),
                })
            .Where(a => a.KeyAttribute != null)
            .OrderBy(a => a.ColumnAttribute == null ? 0 : a.ColumnAttribute.Order)
            .Select(a => a.PropertyInfo)
            .ToArray();

        PropertyInfo[] NonkeyColumnProperties { get; } =
            typeof(TEntity)
            .GetProperties()
            .Where(pi =>
                pi.GetCustomAttribute<KeyAttribute>() == null
                && pi.GetCustomAttribute<ColumnAttribute>() != null
            ).ToArray();

        public TEntity Create()
        {
            return Activator.CreateInstance<TEntity>();
        }

        public TEntity Clone(TEntity entity)
        {
            var clone = Create();

            foreach (var propertyInfo in KeyProperties.Concat(NonkeyColumnProperties))
            {
                propertyInfo.SetValue(clone, propertyInfo.GetValue(entity));
            }

            return clone;
        }

        public object[] KeyValues(TEntity entity)
        {
            return KeyProperties.Select(pi => pi.GetValue(entity)).ToArray();
        }

        public object[] NonkeyValues(TEntity entity)
        {
            return NonkeyColumnProperties.Select(pi => pi.GetValue(entity)).ToArray();
        }

        bool EvaluateIsSurrogateKeyed()
        {
            if (KeyProperties.Length != 1) return false;
            var propertyInfo = KeyProperties[0];
            if (propertyInfo.PropertyType != typeof(long)) return false;

            var dga = propertyInfo.GetCustomAttribute<DatabaseGeneratedAttribute>();
            if (dga != null && dga.DatabaseGeneratedOption == DatabaseGeneratedOption.None)
            {
                return false;
            }

            return true;
        }

        public bool IsSurrogateKeyed { get; }

        public long GetSurrogateKey(TEntity entity)
        {
            if (!IsSurrogateKeyed) throw new InvalidOperationException();

            var propertyInfo = KeyProperties[0];
            return (long)propertyInfo.GetValue(entity); 
        }

        public static bool IsValidSurrogateKey(long id)
        {
            return id >= 1L;
        }

        public long GetOrSetSurrogateKey(TEntity entity, Func<long> generateId)
        {
            if (!IsSurrogateKeyed) throw new InvalidOperationException();

            var propertyInfo = KeyProperties[0];
            var value = (long)propertyInfo.GetValue(entity);
            if (!IsValidSurrogateKey(value))
            {
                value = generateId();
                propertyInfo.SetValue(entity, value);
            }
            return value;
        }

        #region IEntityOperation
        object IEntityOperation.Create() =>
            Create();

        object IEntityOperation.Clone(object entity) =>
            Clone((TEntity)entity);

        object[] IEntityOperation.KeyValues(object entity) =>
            KeyValues((TEntity)entity);

        object[] IEntityOperation.NonkeyValues(object entity) =>
            NonkeyValues((TEntity)entity);
        #endregion

        public EntityOperation()
        {
            IsSurrogateKeyed = EvaluateIsSurrogateKeyed();
        }
    }

    static class EntityOperationModule
    {
        public static IEntityOperation Create(Type entityType)
        {
            var operationType = typeof(EntityOperation<>).MakeGenericType(entityType);
            return (IEntityOperation)Activator.CreateInstance(operationType);
        }
    }
}
