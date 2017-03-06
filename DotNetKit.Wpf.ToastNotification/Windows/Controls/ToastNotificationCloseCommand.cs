using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Input;

namespace DotNetKit.Windows.Controls
{
    /// <summary>
    /// Represents a command to close a toast notification.
    /// </summary>
    public sealed class ToastNotificationCloseCommand
        : ICommand
    {
        /// <summary>
        /// Never occurs.
        /// </summary>
        public event EventHandler CanExecuteChanged
        {
            add { }
            remove { }
        }

        /// <summary>
        /// Gets <c>true</c>.
        /// </summary>
        /// <param name="parameter"></param>
        /// <returns></returns>
        public bool CanExecute(object parameter)
        {
            return true;
        }

        /// <summary>
        /// Closes the toast notification.
        /// </summary>
        /// <param name="parameter"></param>
        public void Execute(object parameter)
        {
            (parameter as ToastNotification)?.Close();
        }

        ToastNotificationCloseCommand()
        {
        }

        /// <summary>
        /// Gets an instance.
        /// </summary>
        public static ToastNotificationCloseCommand Instance { get; } =
            new ToastNotificationCloseCommand();
    }
}
