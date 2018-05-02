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
    /// ProgressContentControl.xaml の相互作用ロジック
    /// </summary>
    [ContentProperty("Child")]
    public partial class ProgressContentControl : UserControl
    {
        readonly DataContextNode childDataContext = new DataContextNode();

        #region ContentVisibility
        public static readonly DependencyProperty ContentVisibilityProperty =
            DependencyProperty.Register(
                "ContentVisibility",
                typeof(Visibility),
                typeof(ProgressContentControl),
                new PropertyMetadata(Visibility.Hidden)
            );

        public Visibility ContentVisibility
        {
            get { return (Visibility)GetValue(ContentVisibilityProperty); }
            set { SetValue(ContentVisibilityProperty, value); }
        }
        #endregion

        #region ProgressVisibility
        public static readonly DependencyProperty ProgressVisibilityProperty =
            DependencyProperty.Register(
                "ProgressVisibility",
                typeof(Visibility),
                typeof(ProgressContentControl),
                new PropertyMetadata(Visibility.Collapsed)
            );

        public Visibility ProgressVisibility
        {
            get { return (Visibility)GetValue(ProgressVisibilityProperty); }
            set { SetValue(ProgressVisibilityProperty, value); }
        }
        #endregion

        #region Child
        public static readonly DependencyProperty ChildProperty =
            DependencyProperty.Register(
                "Child",
                typeof(UIElement),
                typeof(ProgressContentControl),
                new PropertyMetadata()
                {
                    PropertyChangedCallback = OnChildChanged,
                });

        public UIElement Child
        {
            get { return (UIElement)GetValue(ChildProperty); }
            set { SetValue(ChildProperty, value); }
        }

        static void OnChildChanged(DependencyObject sender, DependencyPropertyChangedEventArgs e)
        {
            var @this = (ProgressContentControl)sender;
            var child = e.NewValue as FrameworkElement;
            if (child == null) return;

            child.DataContext = @this.childDataContext;
        }
        #endregion

        #region Task
        public static readonly DependencyProperty TaskProperty =
            DependencyProperty.Register(
                "Task",
                typeof(IProgressTask),
                typeof(ProgressContentControl),
                new PropertyMetadata()
                {
                    PropertyChangedCallback = OnTaskChanged,
                });

        public IProgressTask Task
        {
            get { return (IProgressTask)GetValue(TaskProperty); }
            set { SetValue(TaskProperty, value); }
        }

        IProgressTask previousTask;

        void OnTaskCompleted(IProgressTask task)
        {
            switch (task.TaskNongeneric.Status)
            {
                case TaskStatus.RanToCompletion:
                    {
                        previousTask = task;
                        ProgressVisibility = Visibility.Collapsed;

                        if (!BindingOperations.IsDataBound(contentPresenter, ContentPresenter.ContentProperty))
                        {
                            contentPresenter.SetBinding(
                                ContentPresenter.ContentProperty,
                                new Binding("Child") { Source = this }
                            );
                        }

                        childDataContext.Self = task.ResultNongeneric;
                        break;
                    }
                case TaskStatus.Faulted:
                case TaskStatus.Canceled:
                    Task = previousTask;
                    break;
                default:
                    throw new InvalidOperationException();
            }
        }

        void OnTaskChangedCore(IProgressTask task)
        {
            if (task == previousTask) return;

            if (task.TaskNongeneric.IsCompleted)
            {
                OnTaskCompleted(task);
            }
            else
            {
                ProgressVisibility = Visibility.Visible;

                var context = SynchronizationContext.Current;
                task.TaskNongeneric.ContinueWith(_ =>
                {
                    if (context == null)
                    {
                        OnTaskCompleted(task);
                    }
                    else
                    {
                        context.Post(__ => OnTaskCompleted(task), default(object));
                    }
                });
            }
        }

        static void OnTaskChanged(DependencyObject sender, DependencyPropertyChangedEventArgs e)
        {
            var @this = (ProgressContentControl)sender;
            var task = e.NewValue as IProgressTask;
            if (task == null) throw new InvalidOperationException();

            @this.OnTaskChangedCore(task);
        }
        #endregion

        public ProgressContentControl()
        {
            InitializeComponent();

            BindingOperations.SetBinding(
                childDataContext,
                DataContextNode.ParentProperty,
                new Binding("DataContext") { Source = this }
            );
        }
    }

    public interface IProgressTask
        : INotifyPropertyChanged
    {
        Task TaskNongeneric { get; }
        object ResultNongeneric { get; }

        bool IsIndeterminate { get; }
        double ProgressRate { get; }
    }

    public interface IProgressTask<T>
        : IProgressTask
    {
        Task<T> Task { get; }
    }

    public sealed class DataContextNode
        : DependencyObject
    {
        #region Parent
        public static readonly DependencyProperty ParentProperty =
            DependencyProperty.Register("Parent", typeof(object), typeof(DataContextNode));

        public object Parent
        {
            get { return GetValue(ParentProperty); }
            set { SetValue(ParentProperty, value); }
        }
        #endregion

        #region Self
        public object Self
        {
            get { return GetValue(SelfProperty); }
            set { SetValue(SelfProperty, value); }
        }

        public static readonly DependencyProperty SelfProperty =
            DependencyProperty.Register("Self", typeof(object), typeof(DataContextNode));
        #endregion
    }
}
