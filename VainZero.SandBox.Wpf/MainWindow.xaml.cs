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

            if (stack.Count == 0)
            {
                dictionary.Remove(key);
            }

            value.Dispose();
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

    public sealed class CollectionMergeObservable<TItem, TTarget>
        : IObservable<KeyValuePair<TItem, Notification<TTarget>>>
    {
        public ObservableCollection<TItem> Collection { get; }
        public Func<TItem, IObservable<TTarget>> Func { get; }

        public IDisposable Subscribe(IObserver<KeyValuePair<TItem, Notification<TTarget>>> observer)
        {
            return new CollectionChangedHandler(this, observer).Subscribe();
        }

        sealed class Observer
            : IObserver<TTarget>
            , IDisposable
        {
            readonly CollectionChangedHandler parent;

            readonly TItem item;

            readonly SingleAssignmentDisposable subscription =
                new SingleAssignmentDisposable();

            public Observer(CollectionChangedHandler parent, TItem item)
            {
                this.parent = parent;
                this.item = item;
            }

            KeyValuePair<TItem, Notification<TTarget>> Pair(Notification<TTarget> notification)
            {
                return new KeyValuePair<TItem, Notification<TTarget>>(item, notification);
            }

            bool isCompleted = false;

            public void OnCompleted()
            {
                if (isCompleted) return;

                parent.OnNext(Pair(Notification.CreateOnCompleted<TTarget>()));
                isCompleted = true;
            }

            public void OnError(Exception error)
            {
                parent.OnError(error);
            }

            public void OnNext(TTarget value)
            {
                parent.OnNext(Pair(Notification.CreateOnNext(value)));
            }

            public void Dispose()
            {
                OnCompleted();
            }

            public Observer Subscribe(IObservable<TTarget> observable)
            {
                subscription.Disposable = observable.Subscribe(this);
                return this;
            }
        }

        sealed class CollectionChangedHandler
        {
            readonly CollectionMergeObservable<TItem, TTarget> parent;
            readonly IObserver<KeyValuePair<TItem, Notification<TTarget>>> observer;

            readonly SingleAssignmentDisposable subscription =
                new SingleAssignmentDisposable();

            readonly KeyedCompositeDisposable<TItem, Observer> subscriptions =
                new KeyedCompositeDisposable<TItem, Observer>();

            public CollectionChangedHandler(CollectionMergeObservable<TItem, TTarget> parent, IObserver<KeyValuePair<TItem, Notification<TTarget>>> observer)
            {
                this.parent = parent;
                this.observer = observer;
            }

            void SubscribeItem(TItem item)
            {
                var observable = parent.Func(item);
                subscriptions.Add(item, new Observer(this, item).Subscribe(observable));
            }

            void OnRemoved(TItem item)
            {
                var observer = default(Observer);
                if (subscriptions.Remove(item, out observer))
                {
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
                        SubscribeItem(item);
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
                    subscriptions.Clear();
                }
            }

            public IDisposable Subscribe()
            {
                subscription.Disposable = Disposable.Create(() =>
                {
                    parent.Collection.CollectionChanged -= OnCollectionChanged;
                    subscriptions.Dispose();
                });

                parent.Collection.CollectionChanged += OnCollectionChanged;
                return subscription;
            }

            object Gate => subscription;

            public void OnNext(KeyValuePair<TItem, Notification<TTarget>> value)
            {
                lock (Gate)
                {
                    if (subscription.IsDisposed) return;
                    observer.OnNext(value);
                }
            }

            public void OnError(Exception error)
            {
                lock (Gate)
                {
                    if (subscription.IsDisposed) return;
                    observer.OnError(error);
                    subscription.Dispose();
                }
            }
        }

        public CollectionMergeObservable(ObservableCollection<TItem> collection, Func<TItem, IObservable<TTarget>> func)
        {
            Collection = collection;
            Func = func;
        }
    }

    public enum ItemChangeKind
    {
        OnNext,
        Added,
        Removed,
    }

    public sealed class ItemNotification<T>
    {
        public ItemChangeKind Kind { get; }
        public T Value { get; }

        public ItemNotification(ItemChangeKind kind, T value)
        {
            Kind = kind;
            Value = value;
        }
    }

    public static class ItemChange
    {
        public static ItemNotification<X> CreateAdded<X>(X value)
        {
            return new ItemNotification<X>(ItemChangeKind.Added, value);
        }

        public static ItemNotification<X> CreateRemoved<X>(X value)
        {
            return new ItemNotification<X>(ItemChangeKind.Removed, value);
        }

        public static ItemNotification<X> CreateOnNext<X>(X value)
        {
            return new ItemNotification<X>(ItemChangeKind.OnNext, value);
        }
    }

    public sealed class ForkObservable<TItem, TTarget>
        : IObservable<IObservable<TTarget>>
    {
        public ObservableCollection<TItem> Collection { get; }
        public Func<TItem, IObservable<TTarget>> Func { get; }

        public IDisposable Subscribe(IObserver<IObservable<TTarget>> observer)
        {
            return new CollectionChangedHandler(this, observer).Subscribe();
        }

        sealed class ItemObservable
            : IObservable<TTarget>
            , IDisposable
        {
            sealed class Observer
                : IObserver<TTarget>
            {
                readonly ItemObservable parent;
                readonly IObserver<TTarget> observer;
                readonly SingleAssignmentDisposable subscription = new SingleAssignmentDisposable();

                bool isStopped = false;

                public void OnCompleted()
                {
                    if (isStopped) return;
                    isStopped = true;

                    observer.OnCompleted();
                    subscription.Dispose();

                    Debug.WriteLine("ItemObservable Completed");
                }

                public void OnError(Exception error)
                {
                    if (isStopped) return;
                    isStopped = true;

                    observer.OnError(error);
                    subscription.Dispose();
                }

                public void OnNext(TTarget value)
                {
                    Debug.WriteLine("Value = " + value);
                    observer.OnNext(value);
                }

                public IDisposable Subscribe(IObservable<TTarget> upstream)
                {
                    Debug.WriteLine("ItemObservable is being subscribed.");
                    subscription.Disposable =
                        StableCompositeDisposable.Create(
                            parent.lifetime.Subscribe(this),
                            upstream.Subscribe(this)
                        );
                    return subscription;
                }

                // analyzer: complete-constructor
                public Observer(ItemObservable parent, IObserver<TTarget> observer)
                {
                    if (parent == null)
                        throw new ArgumentNullException(nameof(parent));
                    if (observer == null)
                        throw new ArgumentNullException(nameof(observer));
                    this.parent = parent;
                    this.observer = observer;
                }
            }

            readonly IObservable<TTarget> upstream;

            readonly Subject<TTarget> lifetime = new Subject<TTarget>();

            public void Dispose()
            {
                Debug.WriteLine("ItemObservable.Dispose");
                lifetime.OnCompleted();
            }

            public IDisposable Subscribe(IObserver<TTarget> observer)
            {
                return new Observer(this, observer).Subscribe(upstream);
            }

            // analyzer: complete-constructor
            public ItemObservable(IObservable<TTarget> upstream)
            {
                if (upstream == null)
                    throw new ArgumentNullException(nameof(upstream));
                this.upstream = upstream;
            }
        }

        sealed class CollectionChangedHandler
        {
            readonly ForkObservable<TItem, TTarget> parent;
            readonly IObserver<IObservable<TTarget>> observer;

            readonly SingleAssignmentDisposable subscription =
                new SingleAssignmentDisposable();

            readonly KeyedCompositeDisposable<TItem, ItemObservable> substreams =
                new KeyedCompositeDisposable<TItem, ItemObservable>();

            void OnAdded(TItem item)
            {
                var observable = new ItemObservable(parent.Func(item));
                substreams.Add(item, observable);
                Debug.WriteLine("Added.");
                observer.OnNext(observable);
            }

            void OnRemoved(TItem item)
            {
                var observable = default(ItemObservable);
                if (substreams.Remove(item, out observable))
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

            public IDisposable Subscribe()
            {
                subscription.Disposable = Disposable.Create(() =>
                {
                    parent.Collection.CollectionChanged -= OnCollectionChanged;
                    substreams.Dispose();
                });

                parent.Collection.CollectionChanged += OnCollectionChanged;
                return subscription;
            }

            // analyzer: complete-constructor
            public CollectionChangedHandler(ForkObservable<TItem, TTarget> parent, IObserver<IObservable<TTarget>> observer)
            {
                if (parent == null)
                    throw new ArgumentNullException(nameof(parent));
                if (observer == null)
                    throw new ArgumentNullException(nameof(observer));
                this.parent = parent;
                this.observer = observer;
            }
        }

        public ForkObservable(ObservableCollection<TItem> collection, Func<TItem, IObservable<TTarget>> func)
        {
            Collection = collection;
            Func = func;
        }
    }

    public static class ObservableCollectionExtension
    {
        public static IObservable<IObservable<Y>> Fork<X, Y>(this ObservableCollection<X> @this, Func<X, IObservable<Y>> func)
        {
            return new ForkObservable<X, Y>(@this, func);
        }

        public static IObservable<KeyValuePair<X, Notification<Y>>> ObserveItem<X, Y>(this ObservableCollection<X> @this, Func<X, IObservable<Y>> func)
        {
            return new CollectionMergeObservable<X, Y>(@this, func);
        }

        public static IObservable<Tuple<X, bool, X, bool>> Pairwise2<X>(this IObservable<X> @this)
        {
            return Observable.Create<Tuple<X, bool, X, bool>>(observer =>
            {
                var previous = default(X);
                var previousExists = false;
                return @this.Subscribe(
                    value =>
                    {
                        observer.OnNext(Tuple.Create(previous, previousExists, value, true));
                        previous = value;
                        previousExists = true;
                    },
                    observer.OnError,
                    () =>
                    {
                        if (previousExists)
                        {
                            observer.OnNext(Tuple.Create(previous, previousExists, default(X), false));
                        }
                        observer.OnCompleted();
                    });
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

            /*
            Total =
                Items
                .ObserveItem(item => item.IsChecked.Pairwise2())
                .Scan(0, (count, pair) =>
                {
                    Notification<Tuple<bool, bool, bool, bool>> n = pair.Value;
                    var t = pair.Value.Value;
                    return
                        count
                        + (t.Item2 ? (t.Item1 ? -1 : 0) : 0)
                        + (t.Item4 ? (t.Item3 ? 1 : 0) : 0);
                })
                .ToReactiveProperty(0);
            */

            Total =
                Items
                .Fork(item => item.IsChecked)
                .Select(o => o.Pairwise2())
                .Merge()
                .Scan(0, (count, t) =>
                    count
                    + (t.Item2 ? (t.Item1 ? -1 : 0) : 0)
                    + (t.Item4 ? (t.Item3 ? 1 : 0) : 0)
                )
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
