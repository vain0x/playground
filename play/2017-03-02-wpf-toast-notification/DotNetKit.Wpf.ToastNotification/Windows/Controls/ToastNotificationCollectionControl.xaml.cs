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
    /// Represents a collection control to display notifications.
    /// The DataContext must be a <see cref="ToastNotificationCollection"/>.
    /// </summary>
    public partial class ToastNotificationCollectionControl : UserControl
    {
        /// <summary>
        /// Constructs an instance.
        /// </summary>
        public ToastNotificationCollectionControl()
        {
            InitializeComponent();
        }
    }
}
