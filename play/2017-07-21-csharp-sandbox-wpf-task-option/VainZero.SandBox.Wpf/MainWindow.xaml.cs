using System;
using System.Collections.Generic;
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
using DotNetKit.ErrorHandling;
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

            DataContext = vm;
        }

        sealed class VM
        {
            public ReactiveProperty<string> Message { get; } = new ReactiveProperty<string>("Hello.");
        }

        readonly VM vm = new VM();

        static void PrintContext(string name)
        {
            Debug.WriteLine($"{name}, Thread={Thread.CurrentThread.ManagedThreadId}, Context={SynchronizationContext.Current}");
        }

        private void Button_Click(object sender, RoutedEventArgs e)
        {
            PrintContext("OnClick");

            AddAsync(0)
                .DoOptionAsync(x =>
                {
                    PrintContext("Do1");
                    Debug.WriteLine($"x = {x}");
                })
                .BindOptionAsync((x, context) =>
                {
                    PrintContext("Bind1");
                    return
                        AddAsync(x)
                        .BindOptionAsync(async y =>
                        {
                            await context;
                            PrintContext("Bind2");
                            return await NoneAsync();
                        });
                })
                .DoOptionAsync(x =>
                {
                    PrintContext("Do2");
                    Debug.WriteLine($"x = {x}");
                })
                .ContinueWith(task =>
                {
                    Debug.WriteLine("{0}: {1}", task.Status, task.Status == TaskStatus.RanToCompletion ? (object)task.Result : task.Exception);
                });
        }

        async Task<Option<int>> AddAsync(int x)
        {
            await Task.Delay(100).ConfigureAwait(false);
            Debug.Assert(SynchronizationContext.Current == null);
            return OptionModule.Some(x + 1);
        }

        async Task<Option<int>> NoneAsync()
        {
            await Task.Delay(100).ConfigureAwait(false);
            Debug.Assert(SynchronizationContext.Current == null);
            return Option<int>.None;
        }
    }

    public static class OptionTaskExtension
    {
        public static async Task<Option<Y>>
            MapOptionAsync<X, Y>(this Task<Option<X>> @this, Func<X, Y> func)
        {
            foreach (var x in await @this.ConfigureAwait(false))
            {
                return OptionModule.Some(func(x));
            }
            return Option<Y>.None;
        }

        public static async Task<Option<Y>>
            MapOptionAsync<X, Y>(this Task<Option<X>> @this, Func<X, Task<Option<Y>>> func)
        {
            foreach (var x in await @this.ConfigureAwait(false))
            {
                return await func(x).ConfigureAwait(false);
            }

            return Option<Y>.None;
        }

        public static async Task<Option<Y>>
            MapOptionAsync<X, Y>(this Task<Option<X>> @this, Func<X, SynchronizationContext, Y> func)
        {
            var context = SynchronizationContext.Current;
            foreach (var x in await @this.ConfigureAwait(false))
            {
                return OptionModule.Some(func(x, context));
            }
            return Option<Y>.None;
        }

        public static async Task<Option<Y>>
            MapOptionAsync<X, Y>(this Task<Option<X>> @this, Func<X, SynchronizationContext, Task<Option<Y>>> func)
        {
            var context = SynchronizationContext.Current;
            foreach (var x in await @this.ConfigureAwait(false))
            {
                return await func(x, context).ConfigureAwait(false);
            }

            return Option<Y>.None;
        }

        public static async Task<Option<Y>>
            BindOptionAsync<X, Y>(this Task<Option<X>> @this, Func<X, Task<Option<Y>>> func)
        {
            foreach (var x in await @this.ConfigureAwait(false))
            {
                return await func(x).ConfigureAwait(false);
            }

            return Option<Y>.None;
        }

        public static async Task<Option<Y>>
            BindOptionAsync<X, Y>(this Task<Option<X>> @this, Func<X, SynchronizationContext, Task<Option<Y>>> func)
        {
            var context = SynchronizationContext.Current;
            foreach (var x in await @this.ConfigureAwait(false))
            {
                return await func(x, context).ConfigureAwait(false);
            }

            return Option<Y>.None;
        }

        public static async Task<Option<X>>
            DoOptionAsync<X>(this Task<Option<X>> @this, Action<X> action)
        {
            foreach (var x in await @this.ConfigureAwait(false))
            {
                action(x);
                return OptionModule.Some(x);
            }
            return Option<X>.None;
        }
    }

    public struct SynchronizationContextAwaiter
        : INotifyCompletion
    {
        readonly SynchronizationContext context;

        public bool IsCompleted
        {
            get{ return context == null; }
        }

        public void GetResult()
        {
        }

        public void OnCompleted(Action continuation)
        {
            if (context == null)
            {
                continuation();
            }
            else
            {
                context.Post(k => ((Action)k)(), continuation);
            }
        }

        internal SynchronizationContextAwaiter(SynchronizationContext context)
        {
            this.context = context;
        }
    }

    public static class SynchronizationContextExtension
    {
        public static SynchronizationContextAwaiter GetAwaiter(this SynchronizationContext @this)
        {
            return new SynchronizationContextAwaiter(@this);
        }
    }
}
