using System;
using System.Collections.ObjectModel;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using System.Collections;

namespace DotNetLab.Cs.Wpf.Utility.Detail
{
    public class SelectObservableCollection<TSource, TValue>
        : ObservableCollection<TValue>
        , IDisposable
    {
        ObservableCollection<TSource> Base { get; }
        Func<TSource, TValue> Map { get; }

        IEnumerable<TValue> MapSourceValues(IList sourceValues) =>
            sourceValues.Cast<TSource>().Select(Map);

        void InsertSourceValues(int index, IList sourceValues)
        {
            var i = index;
            foreach (var value in MapSourceValues(sourceValues))
            {
                InsertItem(i, value);
                i++;
            }
        }

        void RemoveItems(int index, int count)
        {
            for (var i = count - 1; i >= 0; i--)
            {
                RemoveItem(index + i);
            }
        }

        void SetItems(int index, IEnumerable<TValue> values)
        {
            var i = index;
            foreach (var value in values)
            {
                SetItem(i, value);
                i++;
            }
        }

        void MoveItems(int sourceIndex, int destinationIndex, int count)
        {
            for (var i = 0; i < count; i++)
            {
                MoveItem(sourceIndex, destinationIndex);

                if (sourceIndex > destinationIndex)
                {
                    sourceIndex++;
                    destinationIndex++;
                }
            }
        }

        public void OnCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    InsertSourceValues(e.NewStartingIndex, e.NewItems);
                    break;
                case NotifyCollectionChangedAction.Remove:
                    RemoveItems(e.OldStartingIndex, e.OldItems.Count);
                    break;
                case NotifyCollectionChangedAction.Replace:
                    SetItems(e.OldStartingIndex, MapSourceValues(e.NewItems));
                    break;
                case NotifyCollectionChangedAction.Move:
                    MoveItems(e.OldStartingIndex, e.NewStartingIndex, e.OldItems.Count);
                    break;
                case NotifyCollectionChangedAction.Reset:
                    Clear();
                    break;
                default:
                    throw new Exception();
            }
        }

        void IDisposable.Dispose()
        {
            Base.CollectionChanged -= OnCollectionChanged;
        }

        public SelectObservableCollection(ObservableCollection<TSource> @base, Func<TSource, TValue> map)
        {
            Base = @base;
            Map = map;

            Base.CollectionChanged += OnCollectionChanged;
            InsertSourceValues(0, Base);
        }
    }
}
