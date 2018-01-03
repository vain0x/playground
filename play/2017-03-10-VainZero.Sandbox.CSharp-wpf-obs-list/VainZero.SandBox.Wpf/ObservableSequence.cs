using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace VainZero.SandBox.Wpf
{
    public class ObservableSequence<TValue>
        : INotifyCollectionChanged
        , IEnumerable<TValue>
    {
        public event NotifyCollectionChangedEventHandler CollectionChanged;

        readonly SynchronizationContext context = SynchronizationContext.Current;
        readonly List<TValue> list = new List<TValue>();

        public void InsertAsync(int index, TValue value)
        {
            list.Insert(index, value);

            context.Post(_ =>
            {
                CollectionChanged?.Invoke(this, new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, value, index));
            }, null);
        }

        public IEnumerator<TValue> GetEnumerator()
        {
            return list.ToList().GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }
}
