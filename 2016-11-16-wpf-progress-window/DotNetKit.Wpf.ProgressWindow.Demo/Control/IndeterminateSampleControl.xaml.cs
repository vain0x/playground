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
    /// IndeterminateSampleControl.xaml の相互作用ロジック
    /// </summary>
    public partial class IndeterminateSampleControl : UserControl
    {
        public IndeterminateSampleControl()
        {
            InitializeComponent();

            var dataContext = new IndeterminateSampleControlViewModel();

            // Open a progress window whenever a task is executed.
            dataContext.TaskStarted += (sender, e) =>
            {
                var task = e.Item1;
                var cancellationTokenSource = e.Item2;
                var owner = Window.GetWindow(this);
                var progressWindow =
                    new ProgressWindowContext()
                    {
                        Task = task,
                        CancellationTokenSource = cancellationTokenSource,
                        Owner = owner,
                        Title = "Indeterminate Sample",
                        Content = "Wait 5 seconds...",
                    };
                progressWindow.ShowDialog();
            };

            DataContext = dataContext;
        }
    }
}
