using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reactive.Concurrency;
using System.Reactive.Disposables;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.Reactive.Threading.Tasks;
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
using Prism.Commands;
using Prism.Mvvm;
using Reactive.Bindings;

using DotNetKit.Reactive.Observables;

namespace VainZero.Sandbox.Wpf
{
    /// <summary>
    /// MainWindow.xaml の相互作用ロジック
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }

        int count;
        ICancelable cancelable;

        private void startButton_Click(object sender, RoutedEventArgs e)
        {
            var id = count++;
            cancelable = new SingleAssignmentDisposable();

            (
                from _1 in Future.Start(() =>
                {
                    Debug.WriteLine($"{id} Subscribed.");
                    return id;
                })
                from _2 in Future.Delay(TimeSpan.FromSeconds(1), TaskPoolScheduler.Default)
                select id
            )
                .Subscribe(value =>
                {
                    if (cancelable.IsDisposed)
                    {
                        Debug.WriteLine($"{id} Canceled.");
                        return;
                    }
                    Debug.WriteLine($"{id} Result = " + value);
                },
                error =>
                {
                    Debug.WriteLine($"{id} Error = " + error.Message);
                },
                () =>
                {
                    Debug.WriteLine($"{id} Completed.");
                });
        }

        private void cancelButton_Click(object sender, RoutedEventArgs e)
        {
            cancelable?.Dispose();
        }
    }
}
