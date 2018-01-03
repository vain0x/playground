using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.Diagnostics;
using System.Linq;
using System.Reactive;
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
using Reactive.Bindings;
using Reactive.Bindings.Extensions;

namespace VainZero.SandBox.Wpf
{
    public sealed class KeyedCompositeDisposable<TKey, TValue>
        : IDisposable
        where TValue : IDisposable
    {
        readonly Dictionary<TKey, Stack<TValue>> dictionary =
            new Dictionary<TKey, Stack<TValue>>();

        public void Add(TKey key, TValue value)
        {
            var stack = default(Stack<TValue>);
            if (dictionary.TryGetValue(key, out stack))
            {
                stack.Push(value);
            }
            else
            {
                stack = new Stack<TValue>();
                stack.Push(value);
                dictionary.Add(key, stack);
            }
        }

        public bool Remove(TKey key, out TValue value)
        {
            var stack = default(Stack<TValue>);
            if (!dictionary.TryGetValue(key, out stack))
            {
                value = default(TValue);
                return false;
            }

            value = stack.Pop();
            value.Dispose();

            if (stack.Count == 0)
            {
                dictionary.Remove(key);
            }
            return true;
        }

        public void Clear()
        {
            foreach (var kv in dictionary)
            {
                foreach (var d in kv.Value)
                {
                    d.Dispose();
                }
            }
            dictionary.Clear();
        }

        public void Dispose()
        {
            Clear();
        }
    }

    public sealed class ObserveItemObservable<TItem, TTarget>
        : IObservable<IObservable<TTarget>>
    {
        sealed class Handler
            : IDisposable
        {
            readonly ObserveItemObservable<TItem, TTarget> parent;
            readonly IObserver<IObservable<TTarget>> observer;

            readonly KeyedCompositeDisposable<TItem, IDisposable> substreams =
                new KeyedCompositeDisposable<TItem, IDisposable>();

            void OnAdded(TItem item)
            {
                var lifetime = new AsyncSubject<Unit>();
                var observable = parent.func(item).TakeUntil(lifetime);
                substreams.Add(item, Disposable.Create(() =>
                {
                    lifetime.OnNext(Unit.Default);
                    lifetime.OnCompleted();
                }));
                Debug.WriteLine("Added.");
                observer.OnNext(observable);
            }

            void OnRemoved(TItem item)
            {
                var disposable = default(IDisposable);
                if (substreams.Remove(item, out disposable))
                {
                    Debug.WriteLine("Removed.");
                }
            }

            void OnCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
            {
                if (e.Action == NotifyCollectionChangedAction.Add
                    || e.Action == NotifyCollectionChangedAction.Replace
                    )
                {
                    foreach (var obj in e.NewItems)
                    {
                        var item = (TItem)obj;
                        OnAdded(item);
                    }
                }

                if (e.Action == NotifyCollectionChangedAction.Replace
                    || e.Action == NotifyCollectionChangedAction.Remove
                    )
                {
                    foreach (var obj in e.OldItems)
                    {
                        var item = (TItem)obj;
                        OnRemoved(item);
                    }
                }

                if (e.Action == NotifyCollectionChangedAction.Reset)
                {
                    substreams.Clear();
                }
            }

            public void Subscribe()
            {
                parent.collection.CollectionChanged += OnCollectionChanged;
            }

            public void Dispose()
            {
                parent.collection.CollectionChanged -= OnCollectionChanged;
                substreams.Dispose();
            }

            // analyzer: complete-constructor
            public Handler(ObserveItemObservable<TItem, TTarget> parent, IObserver<IObservable<TTarget>> observer)
            {
                if (parent == null)
                    throw new ArgumentNullException(nameof(parent));
                if (observer == null)
                    throw new ArgumentNullException(nameof(observer));
                this.parent = parent;
                this.observer = observer;
            }
        }

        readonly ObservableCollection<TItem> collection;
        readonly Func<TItem, IObservable<TTarget>> func;

        public IDisposable Subscribe(IObserver<IObservable<TTarget>> observer)
        {
            var h = new Handler(this, observer);
            h.Subscribe();
            return h;
        }

        public ObserveItemObservable(ObservableCollection<TItem> collection, Func<TItem, IObservable<TTarget>> func)
        {
            this.collection = collection;
            this.func = func;
        }
    }

    public static class ObservableCollectionExtension
    {
        public static IObservable<IObservable<Y>> ObserveItem<X, Y>(this ObservableCollection<X> @this, Func<X, IObservable<Y>> streamSelector)
        {
            return new ObserveItemObservable<X, Y>(@this, streamSelector);
        }

        /// <summary>
        /// [x, y] → [(None, Some(x)), (Some(x), Some(y)), (Some(y), None)]
        /// </summary>
        public static IObservable<Tuple<X, bool, X, bool>> PairwiseWithSentinel<X>(this IObservable<X> @this)
        {
            return Observable.Create<Tuple<X, bool, X, bool>>(observer =>
            {
                var previous = default(X);
                var previousExists = false;
                return
                    @this.Subscribe(
                        value =>
                        {
                            observer.OnNext(Tuple.Create(previous, previousExists, value, true));
                            previous = value;
                            previousExists = true;
                        },
                        observer.OnError,
                        () =>
                        {
                            observer.OnNext(Tuple.Create(previous, previousExists, default(X), false));
                            observer.OnCompleted();
                        });
            });
        }

        public static IObservable<X> Scan<X>(this IObservable<X> @this, X unit, Func<X, X, X> multiply, Func<X, X, X> divide)
        {
            return
                @this.PairwiseWithSentinel().Scan(unit, (accumulate, pair) =>
                {
                    if (pair.Item2)
                    {
                        accumulate = divide(accumulate, pair.Item1);
                    }
                    if (pair.Item4)
                    {
                        accumulate = multiply(accumulate, pair.Item3); 
                    }
                    return accumulate;
                });
        }
    }

    /// <summary>
    /// MainWindow.xaml の相互作用ロジック
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();

            DataContext = this;

            Total =
                Items
                .ObserveItem(item => item.IsChecked)
                .Merge()
                .Select(isChecked => isChecked ? 1 : 0)
                .Scan(0, (x, y) => x + y, (x, y) => x - y)
                .ToReactiveProperty(0);

            Items
                .ObserveElementObservableProperty(item => item.RemoveCommand)
                .Subscribe(pair =>
                {
                    Items.Remove(pair.Instance);
                });

            AddCommand.StartWith(new object[] { null, null, null }).Subscribe(_ =>
            {
                Items.Add(new Item());
            });
        }

        public ObservableCollection<Item> Items { get; } = new ObservableCollection<Item>();

        public ReactiveCommand AddCommand { get; } = new ReactiveCommand();

        public ReactiveProperty<int> Total { get; }
    }

    public sealed class Item
    {
        public ReactiveCommand RemoveCommand { get; } = new ReactiveCommand();
        public ReactiveProperty<bool> IsChecked { get; } = new ReactiveProperty<bool>(false);
    }
}
