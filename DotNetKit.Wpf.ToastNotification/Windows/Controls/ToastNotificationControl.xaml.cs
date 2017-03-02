using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace DotNetKit.Windows.Controls
{
    /// <summary>
    /// Represents a control to display <see cref="ToastNotification"/>.
    /// </summary>
    public partial class ToastNotificationControl : UserControl
    {
        void OnFadeOutCompleted(object sender, EventArgs e)
        {
            var notification = DataContext as ToastNotification;
            if (notification != null)
            {
                notification.Remove();
            }
        }

        /// <summary>
        /// Constructs an instance.
        /// </summary>
        public ToastNotificationControl()
        {
            InitializeComponent();
        }
    }
}
