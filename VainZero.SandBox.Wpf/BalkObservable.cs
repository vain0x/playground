using System;
using System.Collections.Generic;
using System.Linq;
using System.Reactive.Disposables;
using System.Text;
using System.Threading.Tasks;

namespace DotNetKit.Reactive.Observables
{
    sealed class BalkObservable<T>
        : IObservable<T>
    {
        sealed class InnerObserver
            : IObserver<T>
        {
            readonly OuterObserver outer;

            public void OnNext(T value)
            {
                outer.OnNextInner(value);
            }

            public void OnError(Exception error)
            {
                outer.OnErrorInner(error);
            }

            public void OnCompleted()
            {
                outer.OnCompletedInner();
            }

            public InnerObserver(OuterObserver outer)
            {
                this.outer = outer;
            }
        }

        sealed class OuterObserver
            : IObserver<IObservable<T>>
        {
            readonly IObserver<T> downstream;

            readonly SingleAssignmentDisposable subscription =
                new SingleAssignmentDisposable();

            readonly SerialDisposable innerSubscriptionContainer =
                new SerialDisposable();

            bool innerIsStopped = true;
            Exception innerError = null;

            bool outerIsStopped = false;
            Exception outerError = null;

            readonly InnerObserver innerObserver;

            object Gate
            {
                get
                {
                    return innerObserver;
                }
            }

            static Exception Combine(Exception nullable, Exception notNull)
            {
                return
                    nullable == null
                        ? notNull
                        : new AggregateException(nullable, notNull);
            }

            public void OnNext(IObservable<T> value)
            {
                lock (Gate)
                {
                    // Ignore unless the inner completed successfully.
                    if (!(innerIsStopped && innerError == null)) return;

                    innerIsStopped = false;
                }

                var s = new SingleAssignmentDisposable();
                innerSubscriptionContainer.Disposable = s;
                s.Disposable = value.Subscribe(innerObserver);
            }

            public void OnError(Exception error)
            {
                lock (Gate)
                {
                    outerIsStopped = true;
                    outerError = error;

                    if (!innerIsStopped)
                    {
                        // Wait until it stops.
                        // NOTE: If we would like to stop the inner immediately,
                        // we'll need a lock for each value of it and ignore the error of it.
                    }
                    else if (innerError != null)
                    {
                        // Already notified an error.
                    }
                    else
                    {
                        downstream.OnError(error);
                        subscription.Dispose();
                    }
                }
            }

            public void OnCompleted()
            {
                lock (Gate)
                {
                    if (outerIsStopped) return;

                    outerIsStopped = true;
                    outerError = null;

                    if (!innerIsStopped)
                    {
                        // Wait until it stops.
                    }
                    else if (innerError != null)
                    {
                        // Already notified an error.
                    }
                    else
                    {
                        downstream.OnCompleted();
                        subscription.Dispose();
                    }
                }
            }

            public void OnNextInner(T value)
            {
                downstream.OnNext(value);
            }

            public void OnErrorInner(Exception error)
            {
                lock (Gate)
                {
                    innerIsStopped = true;
                    innerError = error;

                    // Notify the error without waiting the outer stops.
                    downstream.OnError(Combine(outerError, innerError));
                    subscription.Dispose();
                }
            }

            public void OnCompletedInner()
            {
                lock (Gate)
                {
                    innerIsStopped = true;
                    innerError = null;

                    if (!outerIsStopped)
                    {
                        // Wait until it stops.
                    }
                    else
                    {
                        if (outerError != null)
                        {
                            downstream.OnError(outerError);
                        }
                        else
                        {
                            downstream.OnCompleted();
                        }

                        subscription.Dispose();
                    }
                }
            }

            public IDisposable Start(IObservable<IObservable<T>> upstream)
            {
                subscription.Disposable =
                    StableCompositeDisposable.Create(
                        innerSubscriptionContainer,
                        upstream.Subscribe(this)
                    );
                return subscription;
            }

            public OuterObserver(IObserver<T> downstream)
            {
                this.downstream = downstream;
                innerObserver = new InnerObserver(this);
            }
        }

        readonly IObservable<IObservable<T>> upstream;

        public IDisposable Subscribe(IObserver<T> observer)
        {
            return new OuterObserver(observer).Start(upstream);
        }

        public BalkObservable(IObservable<IObservable<T>> upstream)
        {
            this.upstream = upstream;
        }
    }
}
