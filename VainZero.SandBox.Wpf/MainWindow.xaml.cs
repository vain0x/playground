using System;
using System.Collections.Generic;
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
using System.Windows.Navigation;
using System.Windows.Shapes;
using Reactive.Bindings;

namespace VainZero.SandBox.Wpf
{
    /// <summary>
    /// MainWindow.xaml の相互作用ロジック
    /// </summary>
    public partial class MainWindow : Window
    {
        ReactiveProperty<TaskControl.MyTask> taskHolder;

        int count = 5;

        void Run()
        {
            var cts = new CancellationTokenSource();
            taskHolder.Value =
                TaskControl.MyTask.Create(
                    Task.Run(async () =>
                    {
                        await Task.Delay(1500);
                        cts.Token.ThrowIfCancellationRequested();
                        count++;
                        return Enumerable.Range(0, count).ToArray();
                    }),
                    cts
                );
        }

        void Button_Click(object sender, RoutedEventArgs e)
        {
            Run();
        }

        public MainWindow()
        {
            InitializeComponent();

            var task = TaskControl.MyTask.Create(Task.FromResult(new int[0]));
            taskHolder = new ReactiveProperty<TaskControl.MyTask>(task);
            DataContext = taskHolder;

            Run();
        }
    }
}
