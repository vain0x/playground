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

using DotNetKit.Windows.Controls;

namespace DotNetKit.Wpf.ToastNotification.Demo
{
    /// <summary>
    /// MainWindow.xaml の相互作用ロジック
    /// </summary>
    public partial class MainWindow : Window
    {
        public new MainWindowViewModel DataContext
        {
            get { return (MainWindowViewModel)base.DataContext; }
            set { base.DataContext = value; }
        }

        public void OnLoad(object sender, EventArgs e)
        {
            var toastNotificationWindow =
                new ToastNotificationWindow(DataContext.ToastNotificationCollection, this);
            toastNotificationWindow.Show();
        }

        public MainWindow()
        {
            InitializeComponent();

            DataContext = new MainWindowViewModel();

            Loaded += OnLoad;
        }
    }
}
