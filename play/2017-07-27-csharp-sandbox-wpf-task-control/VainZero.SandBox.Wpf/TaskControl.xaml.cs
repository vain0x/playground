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
        {
            public Visibility ContentVisibility { get; private set; }

            public Visibility ErrorVisibility { get; private set; }
            public AggregateException Error { get; private set; }

            public Visibility ProgressIndicatorVisibility { get; private set; }
            public ICommand CancelCommand { get; private set; }

            MyState(Visibility contentVisibility, Visibility errorVisibility, AggregateException error, Visibility progressIndicatorVisibility, ICommand cancelCommand)
            {
                ContentVisibility = contentVisibility;
                ErrorVisibility = errorVisibility;
                Error = error;
                ProgressIndicatorVisibility = progressIndicatorVisibility;
                CancelCommand = cancelCommand;
            }

            public MyState ToProgress(ICommand cancelCommand)
            {
                return
                    new MyState(
                        contentVisibility: ContentVisibility,
                        errorVisibility: ErrorVisibility,
                        error: Error,
                        progressIndicatorVisibility: Visibility.Visible,
                        cancelCommand: cancelCommand
                    );
            }

            public static MyState CreateSuccess()
            {
                return
                    new MyState(
                        contentVisibility: Visibility.Visible,
                        errorVisibility: Visibility.Collapsed,
                        error: null,
                        progressIndicatorVisibility: Visibility.Collapsed,
                        cancelCommand: null
                    );
            }

            public static MyState CreateError(AggregateException error)
            {
                return
                    new MyState(
                        contentVisibility: Visibility.Hidden,
                        errorVisibility: Visibility.Visible,
                        error: error,
                        progressIndicatorVisibility: Visibility.Collapsed,
                        cancelCommand: null
                    );
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

        MyState previousState;

        void OnTaskCompleted(MyTask task)
        {
            switch (task.Task.Status)
            {
                case TaskStatus.RanToCompletion:
                    {
                        State = MyState.CreateSuccess();

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
                    State = MyState.CreateError(task.Task.Exception);
                    break;
                case TaskStatus.Canceled:
                    State = previousState;
                    break;
                default:
                    throw new InvalidOperationException();
            }
        }

        void OnTaskChangedCore(MyTask task)
        {
            previousState = State;

            var previousCancelCommand = previousState.CancelCommand;
            if (previousCancelCommand != null && previousCancelCommand.CanExecute(default(object)))
            {
                previousCancelCommand.Execute(default(object));
            }

            if (task.Task.IsCompleted)
            {
                OnTaskCompleted(task);
            }
            else
            {
                State = State.ToProgress(new CancelCommand(task.CancellationTokenSource));

                var context = SynchronizationContext.Current;
                var action = new Action(() =>
                {
                    context.Post(__ => OnTaskCompleted(task), default(object));
                });
                task.Task.ContinueWith(_ => action());

                if (task.CancellationTokenSource != null)
                {
                    task.CancellationTokenSource.Token.Register(() =>
                    {
                        action = () => { };
                        State = previousState;
                    }, useSynchronizationContext: true);
                }
            }
        }

        static void OnTaskChanged(DependencyObject sender, DependencyPropertyChangedEventArgs e)
        {
            var @this = (TaskControl)sender;
            var task = (e.NewValue as MyTask) ?? MyTask.Default;
            @this.OnTaskChangedCore(task);
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

            State = MyState.CreateSuccess();
        }
    }
}
