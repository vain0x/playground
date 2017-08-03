using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reactive.Concurrency;
using System.Reactive.Disposables;
using System.Reactive.Linq;
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
        public MainWindow()
        {
            InitializeComponent();

            ReactivePropertyScheduler.SetDefault(UIDispatcherScheduler.Default);

            var random = new Random();
            var randoms = Enumerable.Range(0, 100).Select(_ => random.Next(10, 100)).ToArray();

            Observable.Range(0, 100)
                .ObserveOn(TaskPoolScheduler.Default)
                .Select(async i =>
                {
                    await Task.Delay(randoms[i]);
                    return i;
                })
                .Concat()
                .ObserveOn(ReactivePropertyScheduler.Default)
                .Subscribe(i => Debug.WriteLine(i));
        }
    }

    public static class ObservableExtension
    {
        public static IObservable<X> CatchRetry<X, E>(this IObservable<X> @this, Action<E> onError)
            where E : Exception
        {
            return
                @this.Catch((E ex) =>
                {
                    onError(ex);
                    return @this.CatchRetry(onError);
                });
        }

        public static IObservable<X> DoOnError<X>(this IObservable<X> @this, Action<Exception> action)
        {
            return
                @this
                .Catch((Exception e) =>
                {
                    action(e);
                    return Observable.Create<X>(observer =>
                    {
                        observer.OnError(e);
                        return Disposable.Empty;
                    });
                });
        }
    }
}
