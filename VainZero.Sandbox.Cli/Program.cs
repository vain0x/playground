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
            Future.FromResult<int, Exception>(1)
                .Map<int, double, Exception, ImmediateSuccessFuture<int, Exception>>(x =>
                {
                    var y = x * Math.PI;
                    Debug.WriteLine(y);
                    return y;
                })
                .Subscribe<MapFlow<int, double, Exception, ImmediateSuccessFuture<int, Exception>>, double, Exception>();
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

    public interface IFlowCallback<TItem, TError>
    {
        void OnItem(TItem item);
        void OnError(TError error);
        void OnCancelled();
        void OnCompleted();
    }

    public interface IFlow<TItem, TError>
    {
        void Subscribe<C>(C callback, ISubscription subscription)
            where C : IFlowCallback<TItem, TError>;
    }

    public interface IOperatorFlow<TSource, TTarget, TError, TUpstream>
        : IFlow<TTarget, TError>
        where TUpstream : IFlow<TSource, TError>
    {
    }

    public interface IFuture<TItem, TError>
        : IFlow<TItem, TError>
    {
    }

    public struct ImmediateSuccessFuture<TItem, TError>
        : IFuture<TItem, TError>
    {
        readonly TItem Item;

        public void Subscribe<C>(C callback, ISubscription subscription)
            where C : IFlowCallback<TItem, TError>
        {
            callback.OnItem(Item);
            callback.OnCompleted();
        }

        public ImmediateSuccessFuture(TItem item)
        {
            Item = item;
        }
    }

    public struct MapFlow<TSource, TTarget, TError, TUpstream>
        : IOperatorFlow<TSource, TTarget, TError, TUpstream>
        where TUpstream : IFlow<TSource, TError>
    {
        public struct Callback<TDownstream>
            : IFlowCallback<TSource, TError>
            where TDownstream : IFlowCallback<TTarget, TError>
        {
            readonly Func<TSource, TTarget> Func;
            readonly TDownstream Downstream;

            public void OnItem(TSource item)
            {
                Downstream.OnItem(Func(item));
            }

            public void OnError(TError error)
            {
                Downstream.OnError(error);
            }

            public void OnCancelled()
            {
                Downstream.OnCancelled();
            }

            public void OnCompleted()
            {
                Downstream.OnCompleted();
            }

            public Callback(TDownstream down, Func<TSource, TTarget> func)
            {
                Downstream = down;
                Func = func;
            }
        }

        readonly TUpstream Upstream;
        readonly Func<TSource, TTarget> Func;

        public void Subscribe<C>(C callback, ISubscription subscription)
            where C : IFlowCallback<TTarget, TError>
        {
            var myCallback = new Callback<C>(callback, Func);
            Upstream.Subscribe(myCallback, subscription);
        }

        public MapFlow(TUpstream upstream, Func<TSource, TTarget> func)
        {
            Upstream = upstream;
            Func = func;
        }
    }

    public struct EmptyFlowCallback<TItem, TError>
        : IFlowCallback<TItem, TError>
    {
        public void OnCancelled()
        {
        }

        public void OnCompleted()
        {
        }

        public void OnError(TError error)
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
        public static ImmediateSuccessFuture<X, E> FromResult<X, E>(X value)
        {
            return new ImmediateSuccessFuture<X, E>(value);
        }
    }

    public static class FlowExtension
    {
        public static void Subscribe<F, X, E>(this F @this)
            where F : IFlow<X, E>
        {
            @this.Subscribe(new EmptyFlowCallback<X, E>(), new Subscription());
        }

        public static MapFlow<S, T, E, U> Map<S, T, E, U>(this U @this, Func<S, T> func)
            where U : IFlow<S, E>
        {
            return new MapFlow<S, T, E, U>(@this, func);
        }
    }
}
