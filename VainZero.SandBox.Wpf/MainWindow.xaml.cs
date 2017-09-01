using System;
using System.Collections.Concurrent;
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

            /*
            Observable.Range(0, 5)
                .ContinueWith(self =>
                    self
                    .Do(i => Debug.WriteLine(i + " on UI thread"))
                    .ObserveOn(TaskPoolScheduler.Default)
                    .Await((i, ct) => Task.Run(async () =>
                    {
                        await Task.Delay(100);
                        return i + 1;
                    }), (i, y) => new { i, y })
                    .Do(i => Debug.WriteLine(i + " on TaskPool"))
                    .Finally(() => Debug.WriteLine("Finally."))
                )
                .Subscribe();*/

            /*
            var subject = new Subject<IObservable<int>>();
            subject.Concat().Subscribe(x =>
            {
                Debug.WriteLine(x);
            });

            for (var i = 0; i < 10; i++)
            {
                var offset = i;
                subject.OnNext(Observable.Create<int>(observer =>
                {
                    Task.Run(() =>
                    {
                        for (var j = 0; j < 10; j++)
                        {
                            observer.OnNext(offset * 100 + j);
                        }
                        observer.OnCompleted();
                    });
                    return Disposable.Empty;
                }));
            }
            */

            var subscription =
                Observable.Create<IObservable<int>>(async (outerObserver, ct) =>
                {
                    for (var i = 0; i < 10; i++)
                    {
                        var offset = i;

                        Debug.WriteLine("OnNext(offset = " + offset + ")");
                        outerObserver.OnNext(Observable.Create<int>(async (innerObserver, ct2) =>
                        {
                            for (var j = 0; j < 10; j++)
                            {
                                innerObserver.OnNext(offset * 10 + j);
                                await Task.Delay(j + 1).ConfigureAwait(false);
                            }

                            Debug.WriteLine("OnCompleted(offset = " + offset + ")");
                            innerObserver.OnCompleted();
                        }));

                        await Task.Delay(10 * (i + 1)).ConfigureAwait(false);
                    }

                    Debug.WriteLine("OnCompleted(outer)");
                    outerObserver.OnCompleted();
                })
                .Balk()
                .Subscribe(
                    x => Debug.WriteLine(x),
                    _ => { },
                    () => Debug.WriteLine("OnCompleted(sink)")
                );
        }
    }



    public sealed class SerialObservable<T>
        : IObservable<T>
    {
        sealed class InnerObserver
            : IObserver<T>
        {
            OuterObserver Outer { get; }

            IObserver<T> Downstream => Outer.Downstream;

            public void OnNext(T value)
            {
                Downstream.OnNext(value);
            }

            public void OnError(Exception error)
            {
                Outer.OnError(error);
            }

            public void OnCompleted()
            {
                Outer.OnCompletedInner();
            }

            public InnerObserver(OuterObserver outer)
            {
                Outer = outer;
            }
        }

        sealed class OuterObserver
            : IObserver<IObservable<T>>
        {
            public IObserver<T> Downstream { get; }
            public SingleAssignmentDisposable Subscription { get; } = new SingleAssignmentDisposable();

            readonly SerialDisposable innerSubscription = new SerialDisposable();

            readonly InnerObserver inner;

            readonly Queue<IObservable<T>> queue = new Queue<IObservable<T>>();
            bool isRunning;
            object Gate => queue;

            // Invoked in a lock.
            void Continue(IObservable<T> observable)
            {
                Debug.WriteLine("Continue.");
                isRunning = true;
                innerSubscription.Disposable = observable.Subscribe(inner);
            }

            public void OnNext(IObservable<T> value)
            {
                lock (Gate)
                {
                    if (isRunning)
                    {
                        Debug.WriteLine("OnNext: Enqueue.");
                        queue.Enqueue(value);
                    }
                    else
                    {
                        Continue(value);
                    }
                }
            }

            public void OnError(Exception error)
            {
                // TODO: inner から同時に呼ばれることもある
                Downstream.OnError(error);
                innerSubscription.Dispose();
                Subscription.Dispose();
            }

            public void OnCompleted()
            {
                // TODO: inner が動いている間は停止しない
                Downstream.OnCompleted();
                innerSubscription.Dispose();
                Subscription.Dispose();
            }

            public void OnCompletedInner()
            {
                lock (Gate)
                {
                    if (queue.Count != 0)
                    {
                        Continue(queue.Dequeue());
                    }
                    else
                    {
                        Debug.WriteLine("Suspended.");
                        innerSubscription.Disposable = Disposable.Empty;
                        isRunning = false;
                    }
                }
            }

            public IDisposable Start(IObservable<IObservable<T>> upstream)
            {
                Subscription.Disposable = upstream.Subscribe(this);
                return Subscription;
            }

            public OuterObserver(IObserver<T> downstream)
            {
                Downstream = downstream;
                inner = new InnerObserver(this);
            }
        }

        IObservable<IObservable<T>> Upstream { get; }

        public IDisposable Subscribe(IObserver<T> observer)
        {
            return new OuterObserver(observer).Start(Upstream);
        }

        public SerialObservable(IObservable<IObservable<T>> upstream)
        {
            Upstream = upstream;
        }
    }

    public static class ObservableExtension
    {
        public static IObservable<X> Balk<X>(this IObservable<IObservable<X>> @this)
        {
            return new BalkObservable<X>(@this);
        }

        public static IObservable<X> Serial<X>(this IObservable<IObservable<X>> @this)
        {
            return new SerialObservable<X>(@this);
        }

        public static IObservable<Y> ContinueWith<X, Y>(this IObservable<X> @this, Func<IObservable<X>, IObservable<Y>> secondFactory)
        {
            var isBusy = false;
            return
                secondFactory(
                    @this.Where(i => isBusy ? false : (isBusy = true))
                )
                .Finally(() => isBusy = false);
        }

        public static IObservable<Z> Await<X, Y, Z>(this IObservable<X> @this, Func<X, CancellationToken, Task<Y>> asyncFunc, Func<X, Y, Z> run)
        {
            return
                @this.Select(x =>
                    Observable.FromAsync(async ct =>
                    {
                        var y = await asyncFunc(x, ct).ConfigureAwait(false);
                        return run(x, y);
                    }))
                .Concat();
        }
    }
}
