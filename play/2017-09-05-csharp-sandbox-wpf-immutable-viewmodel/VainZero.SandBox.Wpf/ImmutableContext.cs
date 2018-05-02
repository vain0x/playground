using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Controls.Primitives;

namespace VainZero.SandBox.Wpf
{
    public static class ImmutableContext
    {
        public static readonly DependencyProperty ItemsSourceProperty =
            DependencyProperty.RegisterAttached(
                "ItemsSource",
                typeof(IEnumerable),
                typeof(ImmutableContext),
                new PropertyMetadata(OnItemsSourceChanged)
            );

        public static IEnumerable GetItemsSource(DependencyObject obj)
        {
            return (IEnumerable)obj.GetValue(ItemsSourceProperty);
        }

        public static void SetItemsSource(DependencyObject obj, IEnumerable value)
        {
            obj.SetValue(ItemsSourceProperty, value);
        }

        static void OnItemsSourceChanged(DependencyObject sender, DependencyPropertyChangedEventArgs e)
        {
            var itemsControl = sender as ItemsControl;
            if (itemsControl == null) throw new InvalidOperationException();

            var collection = itemsControl.ItemsSource as ObservableCollection<object>;
            if (itemsControl.ItemsSource == null)
            {
                collection = new ObservableCollection<object>();
                itemsControl.ItemsSource = collection;
            }

            // 差分を計算する。
            // TODO: ちゃんと実装する。
            var newItems = ((IEnumerable)e.NewValue ?? new object[0]).Cast<object>().ToArray();
            var oldItems = ((IEnumerable)e.OldValue ?? new object[0]).Cast<object>().ToArray();

            System.Diagnostics.Debug.Write("New = [" + string.Join(", ", newItems) + "]");
            System.Diagnostics.Debug.Write("Old = [" + string.Join(", ", oldItems) + "]");

            foreach (var oldItem in oldItems.Except(newItems))
            {
                collection.Remove(oldItem);
            }
            foreach (var newItem in newItems.Except(oldItems))
            {
                collection.Add(newItem);
            }
        }
    }
}
