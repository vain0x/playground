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

namespace DotNetKit.Wpf.Demo
{
    /// <summary>
    /// MinimumSampleControl.xaml の相互作用ロジック
    /// </summary>
    public partial class MinimumSampleControl : UserControl
    {
        async Task RunAsync()
        {
            await Task.Delay(5000);
        }

        void runButton_Click(object sender, RoutedEventArgs e)
        {
            // Create and start a new task which just suspends 5 seconds.
            var task = RunAsync();

            var progressWindow =
                new ProgressWindowContext()
                {
                    Owner = Window.GetWindow(this),
                    Task = task,
                };
            progressWindow.ShowDialog();
        }

        public MinimumSampleControl()
        {
            InitializeComponent();
        }
    }
}
