using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DotNetKit.Reactive.Collections
{
    /// <summary>
    /// Represents a reactive store of immutable array.
    /// </summary>
    /// <typeparam name="T">Type of value.</typeparam>
    [DebuggerDisplay("Count = {Count}")]
    public sealed class ReactiveArray<T>
        : IReactiveArray<T>
    {
        public ReactiveArray(ImmutableArray<T> array)
        {
            _array = array;
        }

        private ImmutableArray<T> _array;

        public void Set(ImmutableArray<T> array)
        {
            _array = array;
            RaiseReset();
        }

        void IReactiveArray.Set(IEnumerable items)
        {
            if (items is ImmutableArray<T>)
            {
                Set((ImmutableArray<T>)items);
            }
            else
            {
                if (items == null)
                    throw new ArgumentNullException("items");

                var array = items.Cast<T>().ToImmutableArray();
                Set(array);
            }
        }

        #region Events
        private static readonly PropertyChangedEventArgs CountChangedEventArg =
            new PropertyChangedEventArgs("Count");

        private static readonly PropertyChangedEventArgs ItemChangedEventArg =
            new PropertyChangedEventArgs("Item[]");

        private static readonly NotifyCollectionChangedEventArgs ResetEventArg =
            new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Reset);

        public event PropertyChangedEventHandler PropertyChanged;

        public event NotifyCollectionChangedEventHandler CollectionChanged;

        private void RaiseReset()
        {
            var propertyChanged = PropertyChanged;
            if (propertyChanged != null)
            {
                propertyChanged(this, CountChangedEventArg);
                propertyChanged(this, ItemChangedEventArg);
            }

            var collectionChanged = CollectionChanged;
            if (collectionChanged != null)
            {
                collectionChanged(this, ResetEventArg);
            }
        }
        #endregion

        #region GetEnumerator
        public struct Enumerator
            : IEnumerator<T>
        {
            internal Enumerator(ImmutableArray<T>.Enumerator enumerator)
            {
                _enumerator = enumerator;
            }

            private ImmutableArray<T>.Enumerator _enumerator;

            public T Current
            {
                get
                {
                    return _enumerator.Current;
                }
            }

            object IEnumerator.Current
            {
                get
                {
                    return Current;
                }
            }

            public bool MoveNext()
            {
                return _enumerator.MoveNext();
            }

            void IEnumerator.Reset()
            {
                throw new NotSupportedException();
            }

            public void Dispose()
            {
            }
        }

        public Enumerator GetEnumerator()
        {
            return new Enumerator(_array.GetEnumerator());
        }
        #endregion

        #region IList<_>
        IEnumerator<T> IEnumerable<T>.GetEnumerator()
        {
            return GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        public T this[int index]
        {
            get
            {
                return _array[index];
            }
            set
            {
                Set(_array.SetItem(index, value));
            }
        }

        public int Count
        {
            get
            {
                return _array.Length;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        public bool IsReadOnly
        {
            get
            {
                return true;
            }
        }

        public void CopyTo(T[] array, int arrayIndex)
        {
            _array.CopyTo(array, arrayIndex);
        }

        public bool Contains(T item)
        {
            return _array.Contains(item);
        }

        public int IndexOf(T item)
        {
            return _array.IndexOf(item);
        }

        public void Add(T item)
        {
            Set(_array.Add(item));
        }

        public void Insert(int index, T item)
        {
            Set(_array.Insert(index, item));
        }

        public bool Remove(T item)
        {
            var array = _array.Remove(item);
            if (_array != array)
            {
                Set(array);
                return true;
            }
            else
            {
                return false;
            }
        }

        public void RemoveAt(int index)
        {
            Set(_array.RemoveAt(index));
        }

        public void Clear()
        {
            Set(ImmutableArray<T>.Empty);
        }
        #endregion

        #region IList
        object IList.this[int index]
        {
            get
            {
                return this[index];
            }
            set
            {
                throw new NotImplementedException();
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        bool IList.IsFixedSize
        {
            get
            {
                return false;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        bool ICollection.IsSynchronized
        {
            get
            {
                return false;
            }
        }

        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        object ICollection.SyncRoot
        {
            get
            {
                throw new NotSupportedException();
            }
        }

        bool IList.Contains(object value)
        {
            return ((IList)_array).Contains(value);
        }

        int IList.IndexOf(object value)
        {
            return ((IList)_array).IndexOf(value);
        }

        void ICollection.CopyTo(Array array, int index)
        {
            ((IList)_array).CopyTo(array, index);
        }

        int IList.Add(object value)
        {
            var index = Count;
            Add((T)value);
            return index;
        }

        void IList.Insert(int index, object value)
        {
            Insert(index, (T)value);
        }

        void IList.Remove(object value)
        {
            Remove((T)value);
        }
        #endregion
    }

    public interface IReactiveArray
        : IList
        , INotifyPropertyChanged
        , INotifyCollectionChanged
    {
        void Set(IEnumerable items);
    }

    public interface IReactiveArray<T>
        : IReadOnlyList<T>
        , IList<T>
        , IReactiveArray
    {
        void Set(ImmutableArray<T> items);
    }
}
