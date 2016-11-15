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
    /// ProgressRateSampleControl.xaml の相互作用ロジック
    /// </summary>
    public partial class ProgressRateSampleControl : UserControl
    {
        public ProgressRateSampleControl()
        {
            InitializeComponent();

            var dataContext = new ProgressRateSampleControlViewModel();

            // Open a progress window whenever a task is executed.
            dataContext.TaskStarted += (sender, e) =>
            {
                var task = e.Item1;
                var cancellationTokenSource = e.Item2;
                var progress = e.Item3;
                var owner = Window.GetWindow(this);
                var progressWindow =
                    new ProgressWindowContext()
                    {
                        Task = task,
                        CancellationTokenSource = cancellationTokenSource,
                        Owner = owner,
                        Title = "ProgressRate Sample",
                        Content = "Wait 5 seconds...",
                    };
                progressWindow.ProgressRate = 0.0;
                progress.ProgressChanged += (_, rate) =>
                {
                    progressWindow.ProgressRate = rate;
                };
                progressWindow.ShowDialog();
            };

            DataContext = dataContext;
        }
    }
}
