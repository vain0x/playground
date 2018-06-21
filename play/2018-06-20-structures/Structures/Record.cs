using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reflection;
using Structures.Internals;

// depends on Opt

namespace Structures
{
    public abstract class RecordBase<TRecord>
        : IEquatable<TRecord>
        where TRecord : RecordBase<TRecord>, new()
    {
        object[] Bag;

        public T Get<T>(Field<T> field)
        {
            return (T)Bag[field.Index];
        }

        public TRecord With<T>(Field<T> field, T value)
        {
            var builder = ToBuilder();
            builder.Set(field, value);
            return builder.IntoRecord();
        }

        #region Builder

        public Builder ToBuilder()
        {
            return new Builder(Bag.ToArray(), Enumerable.Repeat(true, Bag.Length).ToArray());
        }

        public static Builder CreateBuilder()
        {
            var length = GetFieldCount();
            var bag = new object[length];
            var done = new bool[length];
            return new Builder(bag, done);
        }

        public sealed class Builder
        {
            object[] Bag;
            readonly bool[] Done;

            internal Builder(object[] bag, bool[] done)
            {
                Bag = bag;
                Done = done;
            }

            public Builder Set<T>(Field<T> field, T value)
            {
                Bag[field.Index] = value;
                Done[field.Index] = true;
                return this;
            }

            void Verify()
            {
                if (!Done.All(x => x))
                {
                    throw new InvalidOperationException("Missing some fields to be set.");
                }
            }

            public TRecord IntoRecord()
            {
                Verify();

                var record = new TRecord() { Bag = Bag };
                Bag = null;

                return record;
            }
        }

        #endregion

        #region Field

        protected static UntypedField F { get; } = new UntypedField();

        static FieldList Fields = new FieldList(new IRecordField<TRecord>[0], false);

        sealed class FieldList
        {
            public readonly IRecordField<TRecord>[] Fields;
            public readonly bool Frozen;

            public FieldList(IRecordField<TRecord>[] fields, bool frozen)
            {
                Fields = fields;
                Frozen = frozen;
            }
        }

        public sealed class Field<T>
            : RecordFieldBase<TRecord, T>
            , ILens<TRecord, T>
        {

            public Field(Opt<T> defaultValue, int index)
                : base(defaultValue)
            {
            }

            public static implicit operator Field<T>(UntypedField _)
            {
                return Register<T>();
            }

            public T Get(TRecord record)
            {
                return record.Get(this);
            }

            public TRecord With(TRecord record, T value)
            {
                return record.With(this, value);
            }
        }

        internal static Field<T> Register<T>()
        {
            var defaultValue = default(T);

            while (true)
            {
                var oldFields = Fields;
                if (oldFields.Frozen) throw new InvalidOperationException("cannot add field dynamically");

                var index = oldFields.Fields.Length;
                var field = new Field<T>(defaultValue.AcceptNull(), index);
                var fields = new FieldList(oldFields.Fields.Concat(new[] { field }).ToArray(), frozen: false);

                var replaced = System.Threading.Interlocked.CompareExchange(ref Fields, fields, oldFields);
                if (replaced == oldFields) return field;
            }
        }

        static IRecordField<TRecord>[] Freeze()
        {
            while (true)
            {
                var oldFieldList = Fields;
                if (oldFieldList.Frozen) return oldFieldList.Fields;

                var makeFields =
                    typeof(TRecord)
                    .GetProperties(BindingFlags.Static | BindingFlags.GetProperty)
                    .Where(p =>
                        typeof(IRecordField<TRecord>).IsAssignableFrom(p.PropertyType)
                        && p.GetIndexParameters().Length == 0
                        && p.GetMethod != null
                    )
                    .Select(p => p.GetValue(null))
                    .ToVec();
                Debug.WriteLine(makeFields);

                var fieldList = new FieldList(oldFieldList.Fields, frozen: false);
                var replaced = System.Threading.Interlocked.CompareExchange(ref Fields, fieldList, oldFieldList);
                if (replaced == oldFieldList) return fieldList.Fields;
            }
        }

        public static IRecordField<TRecord>[] GetFields()
        {
            return Freeze();
        }

        public static int GetFieldCount()
        {
            return GetFields().Length;
        }

        #endregion

        #region Structural Equality

        static readonly int HashSeed = Guid.NewGuid().GetHashCode();

        public override bool Equals(object obj) =>
            Equals(obj as TRecord);

        public bool Equals(TRecord other)
        {
            if (ReferenceEquals(other, null)) return false;
            if (ReferenceEquals(this, other)) return true;

            for (var i = 0; i < Bag.Length; i++)
            {
                if (!EqualityComparer<object>.Default.Equals(Bag[i], other.Bag[i]))
                    return false;
            }
            return true;
        }

        public override int GetHashCode()
        {
            var h = HashSeed;
            for (var i = 0; i < Bag.Length; i++)
            {
                var x = EqualityComparer<object>.Default.GetHashCode(Bag[i]);
                h = ((int)(((uint)h >> 5) | (uint)h << 27) + h) ^ x;
            }
            return h;
        }

        public static bool operator ==(RecordBase<TRecord> left, RecordBase<TRecord> right)
        {
            return EqualityComparer<RecordBase<TRecord>>.Default.Equals(left, right);
        }

        public static bool operator !=(RecordBase<TRecord> left, RecordBase<TRecord> right)
        {
            return !(left == right);
        }

        #endregion
    }
}

namespace Structures.Internals
{
    public sealed class UntypedField
    {
        public static UntypedField Instance = new UntypedField();
    }

    public interface IRecordField<TRecord>
    {
        int Index { get; }
        Opt<object> DefaultValue { get; }
    }

    public interface IRecordField<TRecord, T>
        : IRecordField<TRecord>
    {
        new Opt<T> DefaultValue { get; }
    }

    public abstract class RecordFieldBase<TRecord, T>
        : IRecordField<TRecord, T>
    {
        public int Index { get; set; }

        public Opt<T> DefaultValue { get; private set; }

        public RecordFieldBase(Opt<T> defaultValue = default(Opt<T>))
        {
            DefaultValue = defaultValue;
        }

        Opt<object> IRecordField<TRecord>.DefaultValue =>
            DefaultValue.Map(x => (object)x);
    }

    public struct RecordFieldFactory<TRecord>
    {
        public static readonly RecordFieldFactory<TRecord> Instance = new RecordFieldFactory<TRecord>();
    }
}
