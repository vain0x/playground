using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using System.Text;
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
        void OnItemRemoved(object sender, EventArgs e)
        {
            Remove((ToastNotification)sender);
        }

        void OnCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            if (e.OldItems != null)
            {
                foreach (var item in e.OldItems.Cast<ToastNotification>())
                {
                    item.Removed -= OnItemRemoved;
                }
            }

            if (e.NewItems != null)
            {
                foreach (var item in e.NewItems.Cast<ToastNotification>())
                {
                    item.Removed += OnItemRemoved;
                }
            }
        }

        /// <summary>
        /// Constructs an instance.
        /// </summary>
        public ToastNotificationCollection()
        {
            CollectionChanged += OnCollectionChanged;
        }
    }
}
