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

            var count = 0;

            StartCommand
                .Select(i =>
                    Observable.FromAsync(async () =>
                    {
                        Status.Value = "実行中 (count = " + count + ")";
                        count++;
                        await Task.Delay(2000);
                        Status.Value = "停止";
                    }))
                .Balk()
                .Subscribe();
        }

        public ReactiveProperty<string> Status { get; } =
            new ReactiveProperty<string>("停止");

        public ReactiveCommand StartCommand { get; } =
            new ReactiveCommand();
    }

    public static class ObservableExtension
    {
        /// <summary>
        /// Balking パターンを用いて、ストリームを平坦化する。
        /// <para>
        /// ストリーム s が流れてくるたび、他のストリームを購読していない場合、s を購読する。s が流す値は、後続に転送される。s が停止したら、購読を解除する。
        /// </para>
        /// </summary>
        public static IObservable<X> Balk<X>(this IObservable<IObservable<X>> @this)
        {
            return new BalkObservable<X>(@this);
        }
    }
}
