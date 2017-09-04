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
    public sealed class KeyedCompositeDisposable<TKey>
        : IDisposable
    {
        readonly Dictionary<TKey, Stack<IDisposable>> dictionary =
            new Dictionary<TKey, Stack<IDisposable>>();

        public void Add(TKey key, IDisposable disposable)
        {
            var stack = default(Stack<IDisposable>);
            if (dictionary.TryGetValue(key, out stack))
            {
                stack.Push(disposable);
            }
            else
            {
                stack = new Stack<IDisposable>();
                stack.Push(disposable);
                dictionary.Add(key, stack);
            }
        }

        public bool Remove(TKey key, out IDisposable disposable)
        {
            var stack = default(Stack<IDisposable>);
            if (!dictionary.TryGetValue(key, out stack))
            {
                disposable = default(IDisposable);
                return false;
            }

            disposable = stack.Pop();

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

            public Observer(CollectionChangedHandler parent, TItem item)
            {
                this.parent = parent;
                this.item = item;
            }

            KeyValuePair<TItem, Notification<TTarget>> Pair(Notification<TTarget> notification)
            {
                return new KeyValuePair<TItem, Notification<TTarget>>(item, notification);
            }

            bool isCompleted;

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
                parent.UnsubscribeItem(item);
            }
        }

        sealed class CollectionChangedHandler
        {
            private CollectionMergeObservable<TItem, TTarget> parent;
            private IObserver<KeyValuePair<TItem, Notification<TTarget>>> observer;
            readonly SingleAssignmentDisposable subscription = new SingleAssignmentDisposable();
            readonly KeyedCompositeDisposable<TItem> subscriptions =
                new KeyedCompositeDisposable<TItem>();

            public CollectionChangedHandler(CollectionMergeObservable<TItem, TTarget> parent, IObserver<KeyValuePair<TItem, Notification<TTarget>>> observer)
            {
                this.parent = parent;
                this.observer = observer;
            }

            void SubscribeItem(TItem item)
            {
                var observable = parent.Func(item);
                subscriptions.Add(item, observable.Subscribe(new Observer(this, item)));
            }

            public void UnsubscribeItem(TItem item)
            {
                var observer = default(IDisposable);
                if (subscriptions.Remove(item, out observer))
                {
                    ((Observer)observer).OnCompleted();
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
                        UnsubscribeItem(item);
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

    public static class ObservableCollectionExtension
    {
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

            Total =
                Items
                .ObserveItem(item => item.IsChecked.Pairwise2())
                .Where(pair => pair.Value.Kind == NotificationKind.OnNext)
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
