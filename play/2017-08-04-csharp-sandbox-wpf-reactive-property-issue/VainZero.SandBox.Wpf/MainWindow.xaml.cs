using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;
using System.Reactive.Concurrency;
using System.Reactive.Disposables;
using System.Reactive.Linq;
using System.Reactive.Subjects;
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
using System.Windows.Threading;
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

            ReactivePropertyScheduler.SetDefault(DispatcherScheduler.Current);

            Items =
                Observable.Timer(TimeSpan.FromSeconds(2), DispatcherScheduler.Current)
                .Select(x =>
                {
                    return new[] { "Japan", "America" };
                })
                .ToRP(new[] { default(string) });

            Selected = Items.Select(xs => xs.FirstOrDefault()).ToRP();

            DataContext = this;
        }

        public MyRP<string[]> Items { get; }
        public MyRP<string> Selected { get; }
    }

    public sealed class MyRP<X>
        : IObservable<X>
        , INotifyPropertyChanged
    {
        X current;
        public X Value
        {
            get { return current; }
            set
            {
                if (EqualityComparer<X>.Default.Equals(current, value)) return;

                current = value;

                // 先にUIの更新通知を発行してから
                ReactivePropertyScheduler.Default.Schedule(() =>
                {
                    PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(nameof(Value)));
                });

                subject.OnNext(value);
            }
        }

        Subject<X> subject = new Subject<X>();
        SingleAssignmentDisposable subscription = new SingleAssignmentDisposable();

        public event PropertyChangedEventHandler PropertyChanged;

        public IDisposable Subscribe(IObserver<X> observer)
        {
            observer.OnNext(current);
            return subject.Subscribe(observer);
        }

        public MyRP(X value, IObservable<X> source)
        {
            current = value;

            subscription.Disposable = source.Subscribe(x => Value = x);
        }
    }

    public static class MyRPExtension
    {
        public static MyRP<X> ToRP<X>(this IObservable<X> @this, X value = default(X))
        {
            return new MyRP<X>(value, @this);
        }
    }
}
