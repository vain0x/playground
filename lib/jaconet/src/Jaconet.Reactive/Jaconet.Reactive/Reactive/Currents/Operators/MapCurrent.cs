using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using Jaconet.Reactive.Listeners;

namespace Jaconet.Reactive.Currents
{
    sealed class MapCurrent<TSource, TTarget>
        : BaseOperatorCurrent<TTarget>
    {
        readonly Current<TSource> source;
        readonly Func<TSource, TTarget> func;

        sealed class MapCurrentListener
            : BaseOperatorCurrentListener<TSource, TTarget>
        {
            readonly MapCurrent<TSource, TTarget> parent;

            public override void OnValue(TSource value)
            {
                var target = default(TTarget);
                try
                {
                    target = parent.func(value);
                }
                catch (Exception e)
                {
                    OnError(e);
                    return;
                }

                Listener.OnValue(target);
            }

            protected override void OnErrorCore(Exception e)
            {
                Listener.OnError(e);
            }

            protected override void OnCompletedCore()
            {
                Listener.OnCompleted();
            }

            public
                MapCurrentListener(
                    MapCurrent<TSource, TTarget> parent,
                    CurrentListener<TTarget> listener,
                    CancellationTokenSource cts
                )
                : base(listener, cts)
            {
                this.parent = parent;
            }
        }

        protected override void
            ConnectCore(
                CurrentListener<TTarget> listener,
                CancellationTokenSource cts
            )
        {
            source.Connect(new MapCurrentListener(this, listener, cts));
        }

        public MapCurrent(Current<TSource> source, Func<TSource, TTarget> func)
        {
            this.source = source;
            this.func = func;
        }
    }
}
