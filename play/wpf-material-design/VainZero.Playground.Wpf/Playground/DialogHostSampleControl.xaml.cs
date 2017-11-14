using System;
using System.Collections.Generic;
using System.Diagnostics;
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
using DotNetKit.Reactive.Disposables;
using MaterialDesignThemes.Wpf;

namespace VainZero.Playground
{
    /// <summary>
    /// DialogHostSampleControl.xaml の相互作用ロジック
    /// </summary>
    public partial class DialogHostSampleControl : UserControl
    {
        public DialogHostSampleControl()
        {
            InitializeComponent();
        }

        #region DialogProperty
        public static readonly DependencyProperty DialogProperty =
            DependencyProperty.RegisterAttached(
                "Dialog",
                typeof(IDialog),
                typeof(DialogHostSampleControl),
                new FrameworkPropertyMetadata()
                {
                    PropertyChangedCallback = OnDialogPropertyChanged,
                });

        public static IDialog GetDialog(DependencyObject obj)
        {
            return (IDialog)obj.GetValue(DialogProperty);
        }

        public static void SetDialog(DependencyObject obj, object value)
        {
            obj.SetValue(DialogProperty, value);
        }

        private static void OnDialogOpened(object sender, DialogOpenedEventArgs eventArgs)
        {
            var host = (DialogHost)sender;
            var dialog = GetDialog(host);

            dialog.OnOpened();
        }

        private static void OnDialogClosing(object sender, DialogClosingEventArgs eventArgs)
        {
            var host = (DialogHost)sender;
            var dialog = GetDialog(host);
            if (dialog == null) return;

            if (eventArgs.IsCancelled) return;
            var cancelable = new AnonymousCancelable(() => eventArgs.IsCancelled, eventArgs.Cancel);

            var reason = (DialogCloseReason)eventArgs.Parameter;

            // TODO: 抽象化
            var frame = (MainSurfaceFrameViewModel)((FrameworkElement)host.Parent).DataContext;
            var closeCommand = frame.CloseDialogCommand;
            var closeRequest = new CloseDialogRequest(dialog, reason, cancelable);

            if (!closeCommand.CanExecute(closeRequest))
            {
                cancelable.Dispose();
                return;
            }

            closeCommand.Execute(closeRequest);

            var host = (DialogHost)sender;
            var dialog = GetDialog(host);

            if (eventArgs.IsCancelled) return;

            // Can cancel.
            dialog.OnClosing(eventArgs.Cancel);

            if (!eventArgs.IsCancelled)
            {
                SetDialog(host, null);
            }
        }

        private static void OnDialogPropertyChanged(DependencyObject sender, DependencyPropertyChangedEventArgs e)
        {
            var host = (DialogHost)sender;
            var oldDialog = (IDialog)e.OldValue;
            var newDialog = (IDialog)e.NewValue;

            if (oldDialog == null && newDialog != null)
            {
                host.DialogOpened += OnDialogOpened;
                host.DialogClosing += OnDialogClosing;
            }

            if (oldDialog != null && newDialog == null)
            {
                host.DialogOpened -= OnDialogOpened;
                host.DialogClosing -= OnDialogClosing;
            }

            /*
            if (oldDialog != null)
            {
                const object Parameter = null;
                DialogHost.CloseDialogCommand.Execute(Parameter, host);

                oldDialog.OnClosed();
            }
            */

            if (newDialog != null)
            {
                host.DialogContent = newDialog;
                host.IsOpen = true;
            }
        }
        #endregion
    }
}
