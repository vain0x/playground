using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Input;
using System.Windows.Media;
using DotNetKit.Functional.Commands;

namespace DotNetKit.Windows.Controls
{
    /// <summary>
    /// Represents a toast notification.
    /// Used as DataContext of notification item.
    /// </summary>
    public abstract class ToastNotification
    {
        /// <summary>
        /// Occurs when the user or system requested to close the notification.
        /// </summary>
        public event EventHandler CloseRequested;

        /// <summary>
        /// Gets the duration to fade out.
        /// </summary>
        public abstract Duration FadeDuration { get; }

        /// <summary>
        /// Requests to close the notification from <see cref="ToastNotificationCollection"/>.
        /// </summary>
        public void Close()
        {
            CloseRequested?.Invoke(this, EventArgs.Empty);
        }

        /// <summary>
        /// Constructs an instance.
        /// </summary>
        protected ToastNotification()
        {
        }
    }
}
