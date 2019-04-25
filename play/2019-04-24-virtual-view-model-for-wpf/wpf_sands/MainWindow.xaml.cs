using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Diagnostics;
using System.Dynamic;
using System.Linq;
using System.Text;
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

namespace wpf_sands
{
    /// <summary>
    /// MainWindow.xaml の相互作用ロジック
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();

            DataContext = new VvmStore<RootViewModel>(setState =>
                new RootViewModel(setState, "hello",
                    new[] { "hello", "world" }.Select((text, i) =>
                          new NoteViewModel(setState, i - 3, text)).ToArray()
                ));
        }
    }

    delegate void SetState<T>(Func<T, T> change);

    interface IVvmNode
    {
        void SetState(object newState);
    }

    enum ObjectKind
    {
        Primitive,
        List,
        Object,
    }

    sealed class ObjectDescriptor
    {
        public ObjectKind GetKind(object value)
        {
            if (value == null || value is string || !value.GetType().IsClass)
                return ObjectKind.Primitive;

            if (value != null && value is System.Collections.IList)
                return ObjectKind.List;

            return ObjectKind.Object;
        }

        public System.Reflection.PropertyInfo[] GetProperties(object value)
        {
            if (value == null)
                return Array.Empty<System.Reflection.PropertyInfo>();

            return value.GetType()
                .GetProperties(System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Instance)
                .Where(p => p.GetIndexParameters().Length == 0)
                .ToArray();
        }
    }

    sealed class DynamicComparer
        : System.Collections.IEqualityComparer
    {
        public new bool Equals(object x, object y)
        {
            if (x == null)
                return y == null;

            if (x is string || !x.GetType().IsClass)
                return x.Equals(y);

            if (x is System.Collections.IList)
            {
                if (!(y is System.Collections.IList))
                    return false;
                var xs = (System.Collections.IList)x;
                var ys = (System.Collections.IList)y;
                if (xs.Count != ys.Count)
                    return false;
                for (var i = 0; i < xs.Count; i++)
                {
                    if (!Equals(xs[i], ys[i]))
                        return false;
                }
                return true;
            }

            if (x.GetType() != y.GetType())
                return false;

            {
                var properties = new ObjectDescriptor().GetProperties(x);
                foreach (var property in properties)
                {
                    var px = property.GetValue(x);
                    var py = property.GetValue(y);
                    if (!Equals(px, py))
                        return false;
                }
                return true;
            }
        }

        public int GetHashCode(object obj)
        {
            throw new NotImplementedException();
        }
    }

    sealed class VvmArray
        : INotifyPropertyChanged
        , INotifyCollectionChanged
        , IVvmNode
        , System.Collections.IList
    {
        object[] Items = Array.Empty<object>();

        public bool IsReadOnly => Items.IsReadOnly;

        public bool IsFixedSize => Items.IsFixedSize;

        public int Count => ((IList)Items).Count;

        public object SyncRoot => Items.SyncRoot;

        public bool IsSynchronized => Items.IsSynchronized;

        public object this[int index] { get => ((IList)Items)[index]; set => ((IList)Items)[index] = value; }

        public event PropertyChangedEventHandler PropertyChanged;
        public event NotifyCollectionChangedEventHandler CollectionChanged;

        void RaisePropertyChanged(string propertyName)
        {
            var h = PropertyChanged;
            if (h != null)
            {
                Debug.WriteLine("PropertyChanged: " + propertyName);
                h(this, new PropertyChangedEventArgs(propertyName));
            }
        }

        void RaiseCollectionChanged(NotifyCollectionChangedEventArgs e)
        {
            var h = CollectionChanged;
            if (h != null)
            {
                Debug.WriteLine("CollectionChanged: " + e.Action.ToString());
                h(this, e);
            }
        }

        object CreateChild(object value)
        {
            if (value == null || value is string || !value.GetType().IsClass || value is ICommand)
                return value;
            if (value is System.Collections.IList)
                return new VvmArray();
            return new VvmObject();
        }

        void SetChild(object oldValue, ref object value, out bool changed)
        {
            if (value == null || value is string || !value.GetType().IsClass ||  value is ICommand)
            {
                changed = !EqualityComparer<object>.Default.Equals(oldValue, value);
                return;
            }

            changed = false;
            if (oldValue == null)
            {
                oldValue = CreateChild(value);
                changed = true;
            }

            if (oldValue is VvmArray)
            {
                // assume value is list
                ((VvmArray)oldValue).SetState(value);
            }
            else
            {
                // assume value is object
                ((VvmObject)oldValue).SetState(value);
            }

            value = oldValue;
        }

        public void SetState(object items)
        {
            var oldItems = Items ?? Array.Empty<object>();
            if (oldItems == items)
                return;

            var newItems = (items as System.Collections.IEnumerable ?? Array.Empty<object>()).Cast<object>().ToArray();
            Items = newItems;

            bool changed;
            var xi = 0;
            var yi = 0;
            var i = 0;

            while (xi < oldItems.Length || yi < newItems.Length)
            {
                if (xi == oldItems.Length || (yi + 1 < newItems.Length && oldItems[xi] == newItems[yi + 1]))
                {
                    SetChild(null, ref Items[yi], out _);

                    // yi is added
                    RaiseCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Add, newItems[yi], i));

                    yi++;
                    i++;
                    continue;
                }

                if (yi == newItems.Length || (xi + 1 < oldItems.Length && oldItems[xi + 1] == newItems[yi]))
                {
                    // xi is removed
                    RaiseCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Remove, oldItems[xi], i));
                    xi++;
                    continue;
                }

                if (oldItems[xi] == newItems[yi])
                {
                    xi++;
                    yi++;
                    i++;
                    continue;
                }

                {
                    SetChild(oldItems[xi], ref Items[yi], out changed);
                    if (changed)
                    {
                        RaiseCollectionChanged(new NotifyCollectionChangedEventArgs(NotifyCollectionChangedAction.Replace, oldItems[xi], items, i));
                    }

                    xi++;
                    yi++;
                    i++;
                }
            }
        }

        public int Add(object value)
        {
            return ((IList)Items).Add(value);
        }

        public bool Contains(object value)
        {
            return ((IList)Items).Contains(value);
        }

        public void Clear()
        {
            ((IList)Items).Clear();
        }

        public int IndexOf(object value)
        {
            return ((IList)Items).IndexOf(value);
        }

        public void Insert(int index, object value)
        {
            ((IList)Items).Insert(index, value);
        }

        public void Remove(object value)
        {
            ((IList)Items).Remove(value);
        }

        public void RemoveAt(int index)
        {
            ((IList)Items).RemoveAt(index);
        }

        public void CopyTo(Array array, int index)
        {
            Items.CopyTo(array, index);
        }

        public IEnumerator GetEnumerator()
        {
            return Items.GetEnumerator();
        }
    }

    class VvmObject
        : DynamicObject
        , INotifyPropertyChanged
    {
        protected object GetState()
        {
            return State;
        }

        object State { get; set; }
        Dictionary<string, object> Children = new Dictionary<string, object>();

        public event PropertyChangedEventHandler PropertyChanged;

        void RaisePropertyChanged(string propertyName)
        {
            var h = PropertyChanged;
            if (h != null)
            {
                Debug.WriteLine("PropertyChanged: " + propertyName);
                h(this, new PropertyChangedEventArgs(propertyName));
            }
        }

        public override bool TryGetMember(GetMemberBinder binder, out object result)
        {
            Debug.WriteLine("TryGetMember: " + binder.Name);

            Children.TryGetValue(binder.Name, out result);
            return true;
        }

        public override bool TrySetMember(SetMemberBinder binder, object value)
        {
            Debug.WriteLine("TrySetMember: " + binder.Name);

            if (State == null)
                return false;

            var property = State.GetType().GetProperty(binder.Name);
            property.SetValue(State, value);
            return true;
        }

        object CreateChild(object value)
        {
            if (value == null || value is string || !value.GetType().IsClass || value is ICommand)
                return value;
            if (value is System.Collections.IList)
                return new VvmArray();
            return new VvmObject();
        }

        void SetChild(object oldValue, ref object value, out bool changed)
        {
            if (value == null || value is string || !value.GetType().IsClass || value is ICommand)
            {
                changed = !EqualityComparer<object>.Default.Equals(oldValue, value);
                return;
            }

            changed = false;
            if (oldValue == null)
            {
                oldValue = CreateChild(value);
                changed = true;
            }

            if (oldValue is VvmArray)
            {
                // assume value is list
                ((VvmArray)oldValue).SetState(value);
            }
            else
            {
                // assume value is object
                ((VvmObject)oldValue).SetState(value);
            }

            value = oldValue;
        }

        public void SetState(object newState)
        {
            if (State == newState)
                return;

            if (newState == null)
            {
                // delete all children
                return;
            }
            Debug.WriteLine("SetState");

            var properties = new ObjectDescriptor().GetProperties(newState);
            Debug.WriteLine("Properties: " + string.Join(", ", properties.Select(p => p.Name)));
            foreach (var property in properties)
            {
                var value = property.GetValue(newState);

                object oldValue;
                var exists = Children.TryGetValue(property.Name, out oldValue);
                bool changed = false;
                SetChild(oldValue, ref value, out changed);

                if (exists)
                {
                    Children[property.Name] = value;
                }
                else
                {
                    Children.Add(property.Name, value);
                }
                if (changed)
                {
                    RaisePropertyChanged(property.Name);
                }
            }

            State = newState;
        }
    }

    sealed class Command<T>
        : ICommand
    {
        readonly Action<T> action;

        public event EventHandler CanExecuteChanged;

        public bool CanExecute(object parameter)
        {
            return true;
        }

        public void Execute(object parameter)
        {
            action((T)parameter);
        }

        public Command(Action<T> action)
        {
            this.action = action;
        }
    }

    sealed class VvmStore<TState>
        : VvmObject
    {
        public VvmStore(Func<SetState<TState>, TState> init)
        {
            var initState = init(change =>
            {
                SetState(change((TState)GetState()));
            });
            SetState(initState);
        }
    }

    sealed class RootViewModel
    {
        readonly SetState<RootViewModel> SetState;

        readonly string text;
        public string Text
        {
            get { return text; }
            set { SetState(root => new RootViewModel(SetState, value, root.Notes)); }
        }

        readonly NoteViewModel[] notes;
        public NoteViewModel[] Notes
        {
            get
            {
                return notes;
            }
        }

        static int NoteId = 0;

        public ICommand AddCommand { get; set; }

        public ICommand RemoveCommand { get; set; }

        public RootViewModel WithNewNote(string text)
        {
            var noteId = ++NoteId;
            return new RootViewModel(SetState, Text, notes.Append(new NoteViewModel(SetState, noteId, text)).ToArray());
        }

        public RootViewModel RemoveNote(int noteId)
        {
            return new RootViewModel(SetState, Text, notes.Where(note => note.NoteId != noteId).ToArray());
        }

        public RootViewModel WithNoteText(int noteId, string text)
        {
            return new RootViewModel(SetState, Text, notes.Select(note =>
                note.NoteId != noteId ? note : note.WithText(text)).ToArray());
        }

        public RootViewModel(SetState<RootViewModel> setState, string text, NoteViewModel[] notes)
        {
            SetState = setState;
            this.text = text;
            this.notes = notes;

            AddCommand = new Command<object>(_ =>
            {
                SetState(root => root.WithNewNote("New Note"));
            });

            RemoveCommand = new Command<int>(noteId =>
            {
                SetState(root => root.RemoveNote(noteId));
            });
        }
    }

    sealed class NoteViewModel
    {
        readonly SetState<RootViewModel> SetState;

        public int NoteId { get; }

        readonly string text;

        public string Text
        {
            get
            {
                return text;
            }
            set
            {
                SetState(root => root.WithNoteText(NoteId, value));
            }
        }

        public NoteViewModel WithText(string text)
        {
            return new NoteViewModel(SetState, NoteId, text);
        }

        public NoteViewModel(SetState<RootViewModel> setState, int noteId, string text)
        {
            SetState = setState;
            NoteId = noteId;
            this.text = text;
        }
    }
}
