using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Markup;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace VainZero.SandBox.Wpf
{
    /// <summary>
    /// TaskControl.xaml の相互作用ロジック
    /// </summary>
    [ContentProperty("Child")]
    public partial class TaskControl : UserControl
    {
        sealed class CancelCommand
            : ICommand
        {
            CancellationTokenSource cts;

            public event EventHandler CanExecuteChanged;

            void TriggerCanExecuteChanged()
            {
                var h = CanExecuteChanged;
                if (h != null) h(this, EventArgs.Empty);
            }

            public bool CanExecute(object parameter)
            {
                return cts != null && !cts.IsCancellationRequested;
            }

            public void Execute(object parameter)
            {
                if (!CanExecute(parameter)) return;

                var localCts = Drop();
                if (localCts != null) localCts.Cancel();
            }

            public CancellationTokenSource Drop()
            {
                if (cts == null) return null;
                var localCts = cts;
                cts = null;
                TriggerCanExecuteChanged();
                return localCts;
            }

            public CancelCommand(CancellationTokenSource cts)
            {
                this.cts = cts.IsCancellationRequested ? null : cts;
            }
        }

        public sealed class MyTask
        {
            readonly Func<object> getResult;

            public Task Task { get; private set; }

            public object Result
            {
                get { return getResult(); }
            }

            public CancellationTokenSource CancellationTokenSource { get; private set; }

            MyTask(Task task, Func<object> getResult, CancellationTokenSource cts)
            {
                Task = task;
                this.getResult = getResult;
                CancellationTokenSource = cts;
            }

            public static MyTask Create(Task task, CancellationTokenSource cts = null)
            {
                return new MyTask(task, () => null, cts);
            }

            public static MyTask Create<X>(Task<X> task, CancellationTokenSource cts = null)
            {
                return new MyTask(task, () => task.Result, cts);
            }

            public static MyTask Default
            {
                get { return Create(Task.FromResult(default(object))); }
            }
        }

        public sealed class MyState
            : INotifyPropertyChanged
        {
            #region INotifyPropertyChanged
            public event PropertyChangedEventHandler PropertyChanged;

            public void SetProperty<X>(ref X field, X value, [CallerMemberName] string propertyName = null)
            {
                if (EqualityComparer<X>.Default.Equals(field, value)) return;

                field = value;
                var h = PropertyChanged;
                if (h != null) h(this, new PropertyChangedEventArgs(propertyName));
            }
            #endregion

            Visibility maskVisibility = Visibility.Collapsed;
            public Visibility MaskVisibility
            {
                get { return maskVisibility; }
                set { SetProperty(ref maskVisibility, value); }
            }

            AggregateException error = null;
            public AggregateException Error
            {
                get { return error; }
                set { SetProperty(ref error, value); }
            }

            Visibility errorVisibility = Visibility.Collapsed;
            public Visibility ErrorVisibility
            {
                get { return errorVisibility; }
                set { SetProperty(ref errorVisibility, value); }
            }

            Visibility progressIndicatorVisibility = Visibility.Collapsed;
            public Visibility ProgressIndicatorVisibility
            {
                get { return progressIndicatorVisibility; }
                set { SetProperty(ref progressIndicatorVisibility, value); }
            }

            ICommand cancelCommand = null;
            public ICommand CancelCommand
            {
                get { return cancelCommand; }
                set { SetProperty(ref cancelCommand, value); }
            }

            public void ChangeToProgress(MyTask task, ICommand cancelCommand)
            {
                MaskVisibility = Visibility.Visible;
                ErrorVisibility = Visibility.Collapsed;
                ProgressIndicatorVisibility = Visibility.Visible;
                CancelCommand = cancelCommand;
            }

            public void ChangeToSuccessful()
            {
                MaskVisibility = Visibility.Collapsed;
                Error = null;
                ErrorVisibility = Visibility.Collapsed;
                ProgressIndicatorVisibility = Visibility.Collapsed;
                CancelCommand = null;
            }

            public void ChangeToError(AggregateException error)
            {
                MaskVisibility = Visibility.Visible;
                Error = error;
                ErrorVisibility = Visibility.Visible;
                ProgressIndicatorVisibility = Visibility.Collapsed;
                CancelCommand = null;
            }

            public void ChangeToCanceled()
            {
                MaskVisibility = Visibility.Collapsed;
                ErrorVisibility = Visibility.Visible;
                ProgressIndicatorVisibility = Visibility.Collapsed;
                CancelCommand = null;
            }
        }

        #region Child
        public static readonly DependencyProperty ChildProperty =
            DependencyProperty.Register("Child", typeof(UIElement), typeof(TaskControl));

        public UIElement Child
        {
            get { return (UIElement)GetValue(ChildProperty); }
            set { SetValue(ChildProperty, value); }
        }
        #endregion

        #region ErrorTemplate
        public static readonly DependencyProperty ErrorTemplateProperty =
            DependencyProperty.Register("ErrorTemplate", typeof(DataTemplate), typeof(TaskControl));

        public DataTemplate ErrorTemplate
        {
            get { return (DataTemplate)GetValue(ErrorTemplateProperty); }
            set { SetValue(ErrorTemplateProperty, value); }
        }
        #endregion

        #region Task
        public static readonly DependencyProperty TaskProperty =
            DependencyProperty.Register(
                "Task",
                typeof(MyTask),
                typeof(TaskControl),
                new PropertyMetadata()
                {
                    PropertyChangedCallback = OnTaskChanged,
                });

        public MyTask Task
        {
            get { return (MyTask)GetValue(TaskProperty); }
            set { SetValue(TaskProperty, value); }
        }

        void OnTaskCompleted(MyTask task)
        {
            switch (task.Task.Status)
            {
                case TaskStatus.RanToCompletion:
                    {
                        State.ChangeToSuccessful();

                        var result = task.Result;
                        var fe = Child as FrameworkElement;
                        if (fe != null)
                        {
                            if (result != null)
                            {
                                fe.DataContext = result;
                            }
                            else
                            {
                                fe.ClearValue(DataContextProperty);
                            }
                        }
                    }
                    break;
                case TaskStatus.Faulted:
                    State.ChangeToError(task.Task.Exception);
                    break;
                case TaskStatus.Canceled:
                    State.ChangeToCanceled();
                    break;
                default:
                    throw new InvalidOperationException();
            }
        }

        static void OnTaskChanged(DependencyObject sender, DependencyPropertyChangedEventArgs e)
        {
            var @this = (TaskControl)sender;
            var task = (e.NewValue as MyTask) ?? MyTask.Default;

            if (task.Task.IsCompleted)
            {
                @this.OnTaskCompleted(task);
            }
            else
            {
                @this.State.ChangeToProgress(task, new CancelCommand(task.CancellationTokenSource));

                var context = SynchronizationContext.Current;
                var action = new Action(() =>
                {
                    context.Post(__ => @this.OnTaskCompleted(task), default(object));
                });
                task.Task.ContinueWith(_ => action());

                if (task.CancellationTokenSource != null)
                {
                    task.CancellationTokenSource.Token.Register(() =>
                    {
                        action = () => { };
                        @this.State.ChangeToCanceled();
                    }, useSynchronizationContext: true);
                }
            }
        }
        #endregion

        #region State
        public static readonly DependencyProperty StateProperty =
            DependencyProperty.Register("State", typeof(MyState), typeof(TaskControl));

        public MyState State
        {
            get { return (MyState)GetValue(StateProperty); }
            set { SetValue(StateProperty, value); }
        }
        #endregion

        public TaskControl()
        {
            InitializeComponent();

            State = new MyState();
        }
    }
}
