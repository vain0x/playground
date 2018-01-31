using System;
using System.Collections.Generic;
using System.Linq;
using System.Reactive;
using System.Reactive.Concurrency;
using System.Reactive.Disposables;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace DotNetKit.Reactive.Observables
{
    public interface IFuture<out T>
        : IObservable<T>
    {
    }

    /// <summary>
    /// a.k.a. Promise
    /// </summary>
    public interface IFutureBuilder<T>
    {
        bool TryResolve(T result);
        bool TryReject(Exception error);
    }

    sealed class FromResultFuture<T>
        : IFuture<T>
    {
        readonly T result;

        public IDisposable Subscribe(IObserver<T> observer)
        {
            observer.OnNext(result);
            observer.OnCompleted();
            return Disposable.Empty;
        }

        public FromResultFuture(T result)
        {
            this.result = result;
        }
    }

    sealed class FromExceptionFuture<T>
        : IFuture<T>
    {
        readonly Exception error;

        public IDisposable Subscribe(IObserver<T> observer)
        {
            observer.OnError(error);
            return Disposable.Empty;
        }

        public FromExceptionFuture(Exception error)
        {
            this.error = error;
        }
    }

    public static class Future
    {
        public static class BooleanFutureCaache
        {
            public static readonly IFuture<bool> True = new FromResultFuture<bool>(true);
            public static readonly IFuture<bool> False = new FromResultFuture<bool>(false);
        }

        public static IFuture<bool> FromResult(bool result)
        {
            return result ? BooleanFutureCaache.True : BooleanFutureCaache.False;
        }

        public static IFuture<X> FromResult<X>(X result)
        {
            return new FromResultFuture<X>(result);
        }

        public static IFuture<X> FromException<X>(Exception ex)
        {
            return new FromExceptionFuture<X>(ex);
        }

        public static IFuture<X> Create<X>(Func<IFutureBuilder<X>, IDisposable> subscribe)
        {
            return new CreateFuture<X>(subscribe);
        }

        public static IFuture<X> Start<X>(Func<X> func)
        {
            return Create<X>(promise =>
            {
                X result;
                try
                {
                    result = func();
                }
                catch (Exception ex)
                {
                    promise.Reject(ex);
                    return Disposable.Empty;
                }

                promise.Resolve(result);
                return Disposable.Empty;
            });
        }

        public static IFuture<Unit> ObserveOn(IScheduler scheduler)
        {
            return Create<Unit>(promise =>
            {
                return scheduler.Schedule(() => promise.Resolve(default(Unit)));
            });
        }

        public static IFuture<Unit> Delay(TimeSpan dueTime, IScheduler scheduler)
        {
            return Create<Unit>(promise =>
            {
                return scheduler.Schedule(dueTime, () => promise.Resolve(default(Unit)));
            });
        }
    }

    public static class FutureExtension
    {
        public static void Resolve<X>(this IFutureBuilder<X> @this, X result)
        {
            @this.TryResolve(result);
        }

        public static void Reject<X>(this IFutureBuilder<X> @this, Exception error)
        {
            @this.TryReject(error);
        }

        public static IFuture<Y> Select<X, Y>(this IFuture<X> @this, Func<X, Y> selector)
        {
            var subscription = new SingleAssignmentDisposable();
            var promise = new FutureBuilder<Y, SingleAssignmentDisposable>(subscription);

            subscription.Disposable =
                @this.Subscribe(
                    value => promise.Resolve(selector(value)),
                    promise.Reject
                );

            return promise;
        }

        public static IFuture<Y> SelectMany<X, Y>(this IFuture<X> @this, Func<X, IFuture<Y>> selector)
        {
            var outerSubscription = new SingleAssignmentDisposable();
            var innerSubscription = new SingleAssignmentDisposable();
            var promise = new FutureBuilder<Y, ICancelable>(StableCompositeDisposable.Create(outerSubscription, innerSubscription));

            outerSubscription.Disposable =
                @this.Subscribe(
                    value =>
                    {
                        try
                        {
                            innerSubscription.Disposable =
                                selector(value).Subscribe(
                                    promise.Resolve,
                                    promise.Reject
                                );
                        }
                        catch (Exception ex)
                        {
                            promise.Reject(ex);
                        }
                    },
                    promise.Reject
                );
            return promise;
        }

        public static IFutureAwaiter<X> GetAwaiter<X>(this IFuture<X> @this)
        {
            return new FutureAwaiter<X>(@this);
        }

        public static IFuture<X> ToFuture<X>(this Task<X> @this)
        {
            var subscription = new SingleAssignmentDisposable();
            var promise = new FutureBuilder<X, SingleAssignmentDisposable>(subscription);
            @this.ContinueWith(localTask =>
            {
                try
                {
                    switch (localTask.Status)
                    {
                        case TaskStatus.RanToCompletion:
                            promise.Resolve(localTask.Result);
                            return;
                        case TaskStatus.Faulted:
                            promise.OnError(localTask.Exception);
                            return;
                        case TaskStatus.Canceled:
                            promise.OnError(new OperationCanceledException());
                            return;
                        default:
                            throw new InvalidOperationException();
                    }
                }
                finally
                {
                    subscription.Dispose();
                }
            }, continuationOptions: TaskContinuationOptions.ExecuteSynchronously);
            return promise;
        }
    }

    sealed class CreateFuture<T>
        : IFuture<T>
    {
        sealed class Promise
            : IFutureBuilder<T>
        {
            readonly IObserver<T> observer;

            readonly SingleAssignmentDisposable subscription;

            bool isStopped = false;

            public bool TryResolve(T result)
            {
                if (isStopped) return false;

                try
                {
                    isStopped = true;
                    observer.OnNext(result);
                    observer.OnCompleted();
                    return true;
                }
                finally
                {
                    subscription.Dispose();
                }
            }

            public bool TryReject(Exception error)
            {
                if (isStopped) return false;

                try
                {
                    isStopped = true;
                    observer.OnError(error);
                    return true;
                }
                finally
                {
                    subscription.Dispose();
                }
            }

            public Promise(IObserver<T> observer, SingleAssignmentDisposable subscription)
            {
                this.observer = observer;
                this.subscription = subscription;
            }
        }

        readonly Func<IFutureBuilder<T>, IDisposable> subscribe;

        public IDisposable Subscribe(IObserver<T> observer)
        {
            var subscription = new SingleAssignmentDisposable();
            var promise = new Promise(observer, subscription);
            try
            {
                subscription.Disposable = subscribe(promise);
            }
            catch (Exception ex)
            {
                promise.Reject(ex);
                return Disposable.Empty;
            }
            return subscription;
        }

        public CreateFuture(Func<IFutureBuilder<T>, IDisposable> subscribe)
        {
            this.subscribe = subscribe;
        }
    }

    public sealed class FutureBuilder<T, TCancelable>
        : IFutureBuilder<T>
        , IFuture<T>
        , ISubject<T, T>
        where TCancelable : ICancelable
    {
        readonly AsyncSubject<T> subject =
            new AsyncSubject<T>();

        readonly TCancelable cancelable;

        public void OnNext(T value)
        {
            subject.OnNext(value);
        }

        public void OnError(Exception error)
        {
            try
            {
                subject.OnError(error);
            }
            finally
            {
                cancelable.Dispose();
            }
        }

        public void OnCompleted()
        {
            try
            {
                subject.OnCompleted();
            }
            finally
            {
                cancelable.Dispose();
            }
        }

        public bool TryResolve(T value)
        {
            if (subject.IsCompleted) return false;
            OnNext(value);
            OnCompleted();
            return true;
        }

        public bool TryReject(Exception error)
        {
            if (subject.IsCompleted) return false;
            OnError(error);
            return true;
        }

        public IDisposable Subscribe(IObserver<T> observer)
        {
            return subject.Subscribe(observer);
        }

        public FutureBuilder(TCancelable cancelable)
        {
            this.cancelable = cancelable;
        }
    }

    public interface IFutureAwaiter<out T>
        : INotifyCompletion
    {
        bool IsCompleted { get; }
        T GetResult();
    }

    public sealed class FutureAwaiter<T>
        : IFutureAwaiter<T>
        , IObserver<T>
    {
        readonly IFuture<T> future;

        readonly SingleAssignmentDisposable subscription =
            new SingleAssignmentDisposable();

        T last = default(T);
        bool lastExists = false;
        Exception ex = null;
        bool isStopped = false;

        Action cont = null;

        public bool IsCompleted
        {
            get
            {
                return isStopped;
            }
        }

        public T GetResult()
        {
            if (!isStopped) throw new InvalidOperationException("Future not stopped.");

            if (lastExists)
            {
                return last;
            }
            else if (ex == null)
            {
                throw new InvalidOperationException("Future emitted no value.");
            }
            else
            {
                // Reraise.
                new AnonymousObserver<T>(_ => { }).OnError(ex);

                throw ex;
            }
        }

        public void OnCompleted(Action continuation)
        {
            cont = continuation;
            subscription.Disposable = future.SubscribeSafe(this);
        }

        public void OnNext(T value)
        {
            last = value;
            lastExists = true;
        }

        public void OnError(Exception error)
        {
            isStopped = true;
            ex = error;
            subscription.Dispose();
        }

        public void OnCompleted()
        {
            isStopped = true;
            try
            {
                cont();
            }
            finally
            {
                subscription.Dispose();
            }
        }

        public FutureAwaiter(IFuture<T> future)
        {
            this.future = future;
        }
    }
}
