using System;
using System.Linq;

namespace Structures
{
    public interface ILens<TSource, T>
    {
        T Get(TSource record);
        TSource With(TSource record, T value);
    }

    public sealed class Lens<TSource, TTarget>
        : ILens<TSource, TTarget>
    {
        readonly Func<TSource, TTarget> get;
        readonly Func<TSource, TTarget, TSource> set;

        public Lens(
            Func<TSource, TTarget> get,
            Func<TSource, TTarget, TSource> set
        )
        {
            this.get = get;
            this.set = set;
        }

        public TTarget Get(TSource source)
        {
            return get(source);
        }

        public TSource With(TSource source, TTarget value)
        {
            return set(source, value);
        }

        public Lens<TSource, U> View<U>(Lens<TTarget, U> sublens)
        {
            return Lens<TSource>.Create(
                s => sublens.get(Get(s)),
                (s, u) => With(s, sublens.With(Get(s), u))
            );
        }
    }

    public static class Lens<TSource>
    {
        public static Lens<TSource, T> Create<T>(Func<TSource, T> get, Func<TSource, T, TSource> set)
        {
            return new Lens<TSource, T>(get, set);
        }

        public static Lens<TSource, T> Iso<T>(Func<TSource, T> convert, Func<T, TSource> convertBack)
        {
            return Create(convert, (_, target) => convertBack(target));
        }

        public static readonly Lens<TSource, TSource> Identity =
            Create(value => value, (_, value) => value);
    }

    public static class Lens
    {
        public static Lens<T[], T> At<T>(int index)
        {
            return Lens<T[]>.Create(
                array => array[index],
                (array, value) =>
                {
                    var copy = array.ToArray();
                    copy[index] = value;
                    return copy;
                });
        }

        public static Lens<T[], T[]> Range<T>(int index, int count)
        {
            return Lens<T[]>.Create(
                array => array.Skip(index).Take(count).ToArray(),
                (array, value) =>
                {
                    var copy = array.ToArray();
                    Array.Copy(value, 0, copy, index, count);
                    return copy;
                });
        }
    }
}
