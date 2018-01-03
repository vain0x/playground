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

            var command = new ReactiveCommand();
            var isActiveActivatable = new ReactivePropertyActivatable();

            command
                .Where(_ => isActiveActivatable.IsActive.Value)
                .Scan(0, (k, _) => k + 1)
                .Do(k =>
                {
                    Debug.WriteLine("Fire: " + k);
                })
                .Subscribe();

            var message =
                isActiveActivatable.IsActive
                .Select(x => x ? "停止" : "開始")
                .ToReactiveProperty();

            DataContext =
                new
                {
                    IsActive = isActiveActivatable.IsActive,
                    Message = message,
                    Command = command,
                };
        }
    }

    public interface IActivatable
    {
        void Activate();
        void Inactivate();
    }

    public sealed class ReactivePropertyActivatable
        : IActivatable
    {
        public ReactiveProperty<bool> IsActive { get; } = new ReactiveProperty<bool>(false);

        public void Activate()
        {
            IsActive.Value = true;
        }

        public void Inactivate()
        {
            IsActive.Value = false;
        }
    }

    public sealed class BooleanActivatable
        : IActivatable
    {
        public bool IsActive { get; private set; } = false;

        public void Activate()
        {
            IsActive = true;
        }

        public void Inactivate()
        {
            IsActive = false;
        }
    }

    public sealed class ActivatableSubscription<T>
        : IActivatable
    {
        readonly IObservable<T> observable;
        SerialDisposable subscription = new SerialDisposable();

        public void Activate()
        {
            subscription.Disposable = observable.Subscribe();
        }

        public void Inactivate()
        {
            subscription.Disposable = Disposable.Empty;
        }

        // analyzer: complete-constructor
        public ActivatableSubscription(IObservable<T> observable)
        {
            if (observable == null)
                throw new ArgumentNullException(nameof(observable));
            this.observable = observable;
        }
    }
}
