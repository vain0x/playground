using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;
using System.Runtime.CompilerServices;
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
        readonly BusyContentControlSample sample;

        private void successButton_Click(object sender, RoutedEventArgs e)
        {
            sample.Restart();
        }

        private void failureButton_Click(object sender, RoutedEventArgs e)
        {
        }

        public MainWindow()
        {
            InitializeComponent();

            DataContext = sample = new BusyContentControlSample();
        }
    }

    public sealed class BusyContentControlSample
    {
        int count = 10;

        public ReactiveProperty<int[]> Items { get; }

        public ReactiveProperty<Task<int[]>> UpdateTask { get; }

        public ReactiveProperty<bool> IsBusy { get; }

        public void Restart()
        {
            count++;

            UpdateTask.Value =
                Task.Run(async () =>
                {
                    await Task.Delay(1000);
                    return Enumerable.Range(0, count).ToArray();
                });
        }

        public BusyContentControlSample()
        {
            var array = Enumerable.Range(0, 10).ToArray();
            Items = new ReactiveProperty<int[]>(array);
            UpdateTask = new ReactiveProperty<Task<int[]>>(Task.FromResult(array));
            IsBusy = new ReactiveProperty<bool>(false);

            UpdateTask.Subscribe(task =>
            {
                IsBusy.Value = true;

                task.ContinueWith(_ =>
                {
                    switch (task.Status)
                    {
                        case TaskStatus.RanToCompletion:
                            Items.Value = task.Result;
                            break;
                        default:
                            break;
                    }
                    IsBusy.Value = false;
                });
            });
        }
    }

    public sealed class ProgressContentControlSample
    {
        public ReactiveProperty<IProgressTask<IReadOnlyList<int>>> ProgressTask { get; }

        int count = 10;

        MyTask FetchAsync()
        {
            count++;
            return new MyTask(pt =>
                Task.Run(async () =>
                {
                    for (var i = 0; i < count; i++)
                    {
                        await Task.Delay(50).ConfigureAwait(false);
                        pt.ProgressRate = (double)(i + 1) / count;
                    }
                    return (IReadOnlyList<int>)Enumerable.Range(0, count).ToArray();
                }));
        }

        public void Restart()
        {
            ProgressTask.Value = FetchAsync();
        }

        public ProgressContentControlSample()
        {
            ProgressTask = new ReactiveProperty<IProgressTask<IReadOnlyList<int>>>(FetchAsync());
        }

        public sealed class MyTask
            : IProgressTask<IReadOnlyList<int>>
            , INotifyPropertyChanged
        {
            public event PropertyChangedEventHandler PropertyChanged;

            void RaisePropertyChanged([CallerMemberName] string propertyName = null)
            {
                var h = PropertyChanged;
                if (h != null) h(this, new PropertyChangedEventArgs(propertyName));
            }

            public Task<IReadOnlyList<int>> Task { get; private set; }

            public bool IsIndeterminate
            {
                get { return false; }
            }

            double rate;
            public double ProgressRate
            {
                get { return rate; }
                set
                {
                    rate = value;
                    RaisePropertyChanged();
                }
            }

            Task IProgressTask.TaskNongeneric
            {
                get { return Task; }
            }

            object IProgressTask.ResultNongeneric
            {
                get { return Task.Result; }
            }

            public MyTask(Func<MyTask, Task<IReadOnlyList<int>>> factory)
            {
                Task = factory(this);
            }
        }
    }
}
