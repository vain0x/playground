using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;

namespace DotNetKit.Wpf
{
    /// <summary>
    /// ProgressWindow.xaml の相互作用ロジック
    /// </summary>
    public partial class ProgressWindow : Window
    {
        readonly ProgressWindowContext dataContext;

        void cancelButton_Click(object sender, RoutedEventArgs e)
        {
            Close();
        }

        void window_Closed(object sender, EventArgs e)
        {
            dataContext.Cancel();
        }

        internal ProgressWindow(ProgressWindowContext dataContext)
        {
            this.dataContext = dataContext;
            DataContext = dataContext;

            InitializeComponent();
        }
    }
}
