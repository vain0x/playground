using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;
using System.Reactive.Concurrency;
using System.Reactive.Disposables;
using System.Reactive.Linq;
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

            Items =
                Observable.Timer(TimeSpan.FromSeconds(2), DispatcherScheduler.Current)
                .Select(x =>
                {
                    return Enumerable.Range(1, 5).Select(i => x + i).ToArray();
                })
                .ToReadOnlyReactiveProperty(new[] { -1L });

            Selected = Items.Select(xs => xs.FirstOrDefault()).ToReactiveProperty();

            DataContext = this;
        }

        public IReadOnlyReactiveProperty<IReadOnlyList<long>> Items { get; }
        public ReactiveProperty<long> Selected { get; }
    }
}
