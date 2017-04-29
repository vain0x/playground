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
using MicroStream.Authentication;
using Reactive.Bindings;

namespace MicroStream
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        static MainWindow()
        {
            ReactivePropertyScheduler.SetDefault(UIDispatcherScheduler.Default);
        }

        public MainWindow()
        {
            InitializeComponent();

            var authenticator = new AuthenticationView(this);
            var mainView = MainView.Create(authenticator);

            DataContext = mainView;

            Loaded += (sender, e) =>
            {
                mainView.Start();
            };
        }
    }
}
