using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.Sandbox
{
    public sealed class Program
    {
        public void Run()
        {
            Future.FromResult(1)
                .Map(x =>
                {
                    var y = x * Math.PI;
                    Debug.WriteLine(y);
                    return y;
                })
                .Subscribe();
        }

        public static void Main(string[] args)
        {
            new Program().Run();
        }
    }

    public interface ISubscription
        : IDisposable
    {
        bool TryDispose();
        bool IsDisposed { get; }
    }

    public interface IFlowCallback<TItem>
    {
        void OnItem(TItem item);
        void OnError(Exception error);
        void OnCancelled();
        void OnCompleted();
    }

    public interface IFlow<TItem>
    {
        void Subscribe<C>(C callback, ISubscription subscription)
            where C : IFlowCallback<TItem>;
    }

    public interface IFuture<TItem>
        : IFlow<TItem>
    {
    }

    sealed class ImmediateSuccessFuture<TItem>
        : IFuture<TItem>
    {
        readonly TItem Item;

        public void Subscribe<C>(C callback, ISubscription subscription)
            where C : IFlowCallback<TItem>
        {
            callback.OnItem(Item);
            callback.OnCompleted();
        }

        public ImmediateSuccessFuture(TItem item)
        {
            Item = item;
        }
    }

    sealed class MapFlow<TSource, TTarget>
        : IFlow<TTarget>
    {
        struct Callback<TDownstream>
            : IFlowCallback<TSource>
            where TDownstream : IFlowCallback<TTarget>
        {
            readonly ISubscription Subscription;
            readonly Func<TSource, TTarget> Func;
            readonly TDownstream Downstream;

            public void OnItem(TSource item)
            {
                Downstream.OnItem(Func(item));
            }

            public void OnError(Exception error)
            {
                Downstream.OnError(error);
                Subscription.Dispose();
            }

            public void OnCancelled()
            {
                Downstream.OnCancelled();
                Subscription.Dispose();
            }

            public void OnCompleted()
            {
                Downstream.OnCompleted();
            }

            public Callback(TDownstream down, ISubscription subscription, Func<TSource, TTarget> func)
            {
                Downstream = down;
                Func = func;
                Subscription = subscription;
            }
        }

        readonly IFlow<TSource> Upstream;
        readonly Func<TSource, TTarget> Func;

        public void Subscribe<C>(C callback, ISubscription subscription)
            where C : IFlowCallback<TTarget>
        {
            var myCallback = new Callback<C>(callback, subscription, Func);
            Upstream.Subscribe(myCallback, subscription);
        }

        public MapFlow(IFlow<TSource> upstream, Func<TSource, TTarget> func)
        {
            Upstream = upstream;
            Func = func;
        }
    }

    public struct EmptyFlowCallback<TItem>
        : IFlowCallback<TItem>
    {
        public void OnCancelled()
        {
        }

        public void OnCompleted()
        {
        }

        public void OnError(Exception error)
        {
        }

        public void OnItem(TItem item)
        {
        }
    }

    public sealed class Subscription
        : ISubscription
    {
        public bool IsDisposed
        {
            get
            {
                return true;
            }
        }

        public void Dispose()
        {
        }

        public bool TryDispose()
        {
            return false;
        }
    }

    public static class Future
    {
        public static IFuture<X> FromResult<X>(X value)
        {
            return new ImmediateSuccessFuture<X>(value);
        }
    }

    public static class FlowExtension
    {
        public static void Subscribe<X>(this IFlow<X> @this)
        {
            @this.Subscribe(new EmptyFlowCallback<X>(), new Subscription());
        }

        public static IFlow<T> Map<S, T>(this IFlow<S> @this, Func<S, T> func)
        {
            return new MapFlow<S, T>(@this, func);
        }
    }
}
