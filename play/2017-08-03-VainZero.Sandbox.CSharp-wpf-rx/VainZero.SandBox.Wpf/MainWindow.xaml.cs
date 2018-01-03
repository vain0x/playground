using System;
using System.Collections.Generic;
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

            ReactivePropertyScheduler.SetDefault(DispatcherScheduler.Current);

            var context = SynchronizationContext.Current;

            Task.Run(async () =>
            {
                Debug.WriteLine(Thread.CurrentThread.ManagedThreadId);
                await Task.Delay(100);
                Debug.WriteLine(Thread.CurrentThread.ManagedThreadId);
                await ReactivePropertyScheduler.Default;
                Debug.WriteLine(Thread.CurrentThread.ManagedThreadId);
                await ReactivePropertyScheduler.Default;
                Debug.WriteLine(Thread.CurrentThread.ManagedThreadId);
                await TaskPoolScheduler.Default;
                Debug.WriteLine(Thread.CurrentThread.ManagedThreadId);
                await TaskPoolScheduler.Default;
                Debug.WriteLine(Thread.CurrentThread.ManagedThreadId);
                await ReactivePropertyScheduler.Default;
                Debug.WriteLine(Thread.CurrentThread.ManagedThreadId);
                await ImmediateScheduler.Instance;
                Debug.WriteLine(Thread.CurrentThread.ManagedThreadId);
            });

            /*
            Invoke(async () =>
            {
                await Task.Delay(1000);
                Debug.WriteLine(1);
                throw new Exception("error");
            })
            .ContinueWith(task =>
            {
                Debug.WriteLine(task);
            });

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
                .Subscribe(i => Debug.WriteLine(i));*/
        }

        static X Invoke<X>(Func<X> fun)
        {
            return fun();
        }

        public static async Task<X> TaskFromSynchronizationContext<X>(SynchronizationContext context, Func<X> func)
        {
            await context;
            return func();
        }
    }

    /// <summary>
    /// await したときに継続をスケジューラーでスケジュールする awaiter を表す。
    /// NOTE: 実装上、DispatcherScheduler に対して起動されることが多いので、
    /// それに対する最適化を施している。
    /// </summary>
    public struct SchedulerAwaiter
        : INotifyCompletion
    {
        readonly IScheduler scheduler;
        readonly Dispatcher dispatcherOrNull;

        public bool IsCompleted
        {
            get
            {
                return
                    (dispatcherOrNull != null && dispatcherOrNull.CheckAccess())
                    || scheduler == ImmediateScheduler.Instance;
            }
        }

        public void GetResult()
        {
        }

        public void OnCompleted(Action continuation)
        {
            if (dispatcherOrNull != null)
            {
                dispatcherOrNull.BeginInvoke(continuation);
                return;
            }

            if (scheduler == ImmediateScheduler.Instance)
            {
                continuation();
                return;
            }

            scheduler.Schedule(continuation);
        }

        static Dispatcher DispatcherOrNull(IScheduler scheduler)
        {
            var ds = scheduler as DispatcherScheduler;
            if (ds == null) return null;

            return ds.Dispatcher;
        }

        public SchedulerAwaiter(IScheduler scheduler)
        {
            this.scheduler = scheduler;
            dispatcherOrNull = DispatcherOrNull(scheduler);
        }
    }

    public static class SchedulerExtension
    {
        public static SchedulerAwaiter GetAwaiter(this IScheduler @this)
        {
            return new SchedulerAwaiter(@this);
        }

        public static void Join(this IScheduler scheduler)
        {
            var resetEvent = new ManualResetEventSlim();
            scheduler.Schedule(resetEvent.Set);
            resetEvent.Wait();
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

    public static class SynchronizationContextExtension
    {
        public static SynchronizationContextAwaiter GetAwaiter(this SynchronizationContext @this)
        {
            return new SynchronizationContextAwaiter(@this);
        }
    }

    public struct SynchronizationContextAwaiter
        : INotifyCompletion
    {
        readonly SynchronizationContext context;

        public bool IsCompleted
        {
            get
            {
                return context == null || SynchronizationContext.Current == context;
            }
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
}
