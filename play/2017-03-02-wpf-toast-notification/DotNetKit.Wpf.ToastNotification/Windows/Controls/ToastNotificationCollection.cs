using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Input;

namespace DotNetKit.Windows.Controls
{
    /// <summary>
    /// Represents a collection of toast notifications.
    /// </summary>
    public class ToastNotificationCollection
        : ObservableCollection<ToastNotification>
    {
        #region Synchronization
        readonly SynchronizationContext context;

        void Invoke(Action action)
        {
            if (context == null || context == SynchronizationContext.Current)
            {
                action();
            }
            else
            {
                context.Send(_ => action(), null);
            }
        }

        /// <summary>
        /// Clears all items in the captured synchronization context.
        /// </summary>
        protected override void ClearItems()
        {
            Invoke(() => base.ClearItems());
        }

        /// <summary>
        /// Inserts an item in the captured synchronization context.
        /// </summary>
        /// <param name="index"></param>
        /// <param name="item"></param>
        protected override void InsertItem(int index, ToastNotification item)
        {
            Invoke(() => base.InsertItem(index, item));
        }

        /// <summary>
        /// Removes an item in the captured synchronization context.
        /// </summary>
        /// <param name="index"></param>
        protected override void RemoveItem(int index)
        {
            Invoke(() => base.RemoveItem(index));
        }

        /// <summary>
        /// Sets an item in the captured synchronization context.
        /// </summary>
        /// <param name="index"></param>
        /// <param name="item"></param>
        protected override void SetItem(int index, ToastNotification item)
        {
            Invoke(() => base.SetItem(index, item));
        }

        /// <summary>
        /// Moves an item in the captured synchronization context.
        /// </summary>
        /// <param name="oldIndex"></param>
        /// <param name="newIndex"></param>
        protected override void MoveItem(int oldIndex, int newIndex)
        {
            Invoke(() => base.MoveItem(oldIndex, newIndex));
        }
        #endregion

        void OnCloseRequested(object sender, EventArgs e)
        {
            Remove((ToastNotification)sender);
        }

        void OnCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            if (e.OldItems != null)
            {
                foreach (var item in e.OldItems.Cast<ToastNotification>())
                {
                    item.CloseRequested -= OnCloseRequested;
                }
            }

            if (e.NewItems != null)
            {
                foreach (var item in e.NewItems.Cast<ToastNotification>())
                {
                    item.CloseRequested += OnCloseRequested;
                }
            }
        }

        /// <summary>
        /// Constructs an instance.
        /// </summary>
        public ToastNotificationCollection()
        {
            context = SynchronizationContext.Current;

            CollectionChanged += OnCollectionChanged;
        }
    }
}
