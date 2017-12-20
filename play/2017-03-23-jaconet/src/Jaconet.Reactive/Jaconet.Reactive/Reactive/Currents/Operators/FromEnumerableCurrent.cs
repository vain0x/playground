using System;
using System.Collections.Generic;
using System.Text;
using Jaconet.Reactive.Listeners;

namespace Jaconet.Reactive.Currents
{
    sealed class FromEnumerableCurrent<TValue>
        : BaseCurrent<TValue>
    {
        readonly IEnumerable<TValue> enumerable;

        public override void Connect(CurrentListener<TValue> listener)
        {
            listener.CancellationToken.ThrowIfCancellationRequested();

            switch (enumerable)
            {
                case IList<TValue> list:
                    var count = list.Count;
                    for (var i = 0; i < count; i++)
                    {
                        listener.CancellationToken.ThrowIfCancellationRequested();

                        var value = default(TValue);
                        var valueExists = default(bool);
                        try
                        {
                            value = list[i];
                            valueExists = true;
                        }
                        catch (Exception e)
                        {
                            listener.OnError(e);
                            return;
                        }

                        if (!valueExists)
                        {
                            listener.OnCompleted();
                            return;
                        }

                        listener.OnValue(value);
                    }
                    break;
                default:
                    var enumerator = default(IEnumerator<TValue>);
                    try
                    {
                        enumerator = enumerable.GetEnumerator();
                    }
                    catch (Exception e)
                    {
                        listener.OnError(e);
                        return;
                    }

                    while (true)
                    {
                        listener.CancellationToken.ThrowIfCancellationRequested();

                        var value = default(TValue);
                        var valueExists = default(bool);
                        try
                        {
                            if (enumerator.MoveNext())
                            {
                                value = enumerator.Current;
                                valueExists = true;
                            }
                        }
                        catch (Exception e)
                        {
                            listener.OnError(e);
                            return;
                        }

                        if (!valueExists)
                        {
                            listener.OnCompleted();
                            return;
                        }

                        listener.OnValue(value);
                    };
            }
        }

        public FromEnumerableCurrent(IEnumerable<TValue> enumerable)
        {
            this.enumerable = enumerable;
        }
    }
}
