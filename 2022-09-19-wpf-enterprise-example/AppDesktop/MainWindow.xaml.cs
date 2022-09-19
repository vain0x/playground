using AppCore;
using System.Windows;

namespace AppDesktop
{
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();

            Loaded += (_sender, _e) =>
            {
                SQLitePCL.Batteries.Init();
                AppDatabase.Example().GetAwaiter().GetResult();
            };
        }
    }
}
