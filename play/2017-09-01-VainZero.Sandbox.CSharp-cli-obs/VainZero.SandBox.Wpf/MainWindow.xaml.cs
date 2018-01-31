using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reactive;
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
using DotNetKit.Reactive.Observables;
using Reactive.Bindings;

namespace VainZero.SandBox.Wpf
{
    /// <summary>
    /// MainWindow.xaml の相互作用ロジック
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();

            DataContext = this;

            StartCommand
                .DoBalking(async () =>
                {
                    Status.Value = "実行中";
                    await Task.Delay(3000);
                    Status.Value = "停止";
                })
                .Subscribe(_ =>
                {
                    Count.Value++;
                });
        }

        public ReactiveProperty<int> Count { get; } =
            new ReactiveProperty<int>(0);

        public ReactiveProperty<string> Status { get; } =
            new ReactiveProperty<string>("停止");

        public ReactiveCommand StartCommand { get; } =
            new ReactiveCommand();
    }

    public static class ObservableExtension
    {
        public static IObservable<X> Balk<X>(this IObservable<IObservable<X>> @this)
        {
            return new BalkObservable<X>(@this);
        }

        public static IObservable<Unit> DoBalking<X>(this IObservable<X> @this, Func<Task> asyncFunc)
        {
            var observable = Observable.FromAsync(asyncFunc);
            return @this.Select(_ => observable).Balk();
        }
    }
}
