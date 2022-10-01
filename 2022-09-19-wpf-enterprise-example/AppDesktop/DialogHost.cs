using System;
using System.ComponentModel;
using System.Diagnostics;
using System.Windows.Data;
using System.Windows;
using System.Windows.Markup;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;

namespace AppDesktop
{
    [ContentProperty(nameof(ContentTemplate))]
    internal sealed class DialogHost : FrameworkElement
    {
        // UI要素にダイアログを持たせるための添付プロパティ
        public static DependencyProperty DialogHostProperty { get; } = DependencyProperty.RegisterAttached(
            "DialogHost",
            typeof(DialogHost),
            typeof(DialogHost),
            new PropertyMetadata
            {
                PropertyChangedCallback = OnDialogHostPropertyChanged,
            });

        public static DialogHost? GetDialogHost(DependencyObject obj) => (DialogHost?)obj.GetValue(DialogHostProperty);
        public static void SetDialogHost(DependencyObject obj, DialogHost? value) => obj.SetValue(DialogHostProperty, value);

        // ダイアログが開いているかどうか
        public static DependencyProperty IsOpenProperty { get; } = DependencyProperty.Register(
            nameof(IsOpen),
            typeof(bool),
            typeof(DialogHost),
            new PropertyMetadata
            {
                DefaultValue = false,
                PropertyChangedCallback = IsOpenChangedCallback,
            });

        public bool IsOpen
        {
            get => (bool)GetValue(IsOpenProperty);
            set => SetValue(IsOpenProperty, value);
        }

        // ダイアログの中身の値 (Window.DataContxt)
        public static DependencyProperty ContentProperty { get; } = DependencyProperty.Register(
            nameof(Content),
            typeof(object),
            typeof(DialogHost)
        );

        public object? Content
        {
            get => GetValue(ContentProperty);
            set => SetValue(ContentProperty, value);
        }

        // ダイアログの中身のテンプレート (Window.ContentTemplate)
        public static DependencyProperty ContentTemplateProperty { get; } = DependencyProperty.Register(
            nameof(ContentTemplate),
            typeof(DataTemplate),
            typeof(DialogHost)
        );

        public DataTemplate? ContentTemplate
        {
            get => (DataTemplate?)GetValue(ContentTemplateProperty);
            set => SetValue(ContentTemplateProperty, value);
        }

        // ライフサイクル
        public event EventHandler? Opening;
        //public event EventHandler? Closed;

        // (内部利用) DialogHostから属性の持ち主へのリンク
        public static DependencyProperty AttributeParentProperty { get; } = DependencyProperty.Register(
            nameof(AttributeParent),
            typeof(DependencyObject),
            typeof(DialogHost)
        );

        public DependencyObject? AttributeParent
        {
            get => (DependencyObject?)GetValue(AttributeParentProperty);
            set => SetValue(AttributeParentProperty, value);
        }

        // (内部利用) ダイアログから開いているDialogHostへのリンク
        public static DependencyProperty OpenerProperty { get; } =
            DependencyProperty.RegisterAttached("Opener", typeof(DialogHost), typeof(DialogHost));

        public static DialogHost? GetOpener(DependencyObject obj) => (DialogHost?)obj.GetValue(OpenerProperty);
        public static void SetOpener(DependencyObject obj, DialogHost? value) => obj.SetValue(OpenerProperty, value);

        private Window? window;

        private static void OnDialogHostPropertyChanged(DependencyObject d, DependencyPropertyChangedEventArgs e)
        {
            Debug.WriteLine($"*.DialogHost changed");

            // 属性の持ち主のDataContextをthisに伝播させる
            if (e.NewValue is DialogHost host)
            {
                host.SetBinding(DataContextProperty, BindingOn(d, nameof(DataContext)));
                host.AttributeParent = d;
            }
            else if (e.OldValue is DialogHost oldHost)
            {
                BindingOperations.ClearBinding(oldHost, DataContextProperty);
                oldHost.AttributeParent = null;
            }
        }

        private static void OnWindowClosing(object? sender, CancelEventArgs e)
        {
            // IsOpenがfalseになるまで閉じない
            e.Cancel = true;

            //var host = GetOpener((Window)sender!)!;
            //host.RequestClose();

            // Escキーをシミュレートする
            var device = Keyboard.PrimaryDevice;
            InputManager.Current.ProcessInput(new KeyEventArgs(device, device.ActiveSource, 0, Key.Escape)
            {
                RoutedEvent = KeyDownEvent
            });
        }

        private static void OnWindowClosed(object? sender, EventArgs _e)
        {
            var window = (Window)sender!;
            SetOpener(window, null);
            BindingOperations.ClearAllBindings(window);
        }

        private void OnUnloaded(object? sender, EventArgs _e)
        {
            if (window != null)
            {
                window.Close();
                window = null;
            }
        }

        private static void IsOpenChangedCallback(DependencyObject sender, DependencyPropertyChangedEventArgs e)
        {
            var host = (DialogHost)sender;
            var isOpen = (bool)e.NewValue;

            sender.Dispatcher.InvokeAsync(new Action(() =>
            {
                host.OnIsOpenChanged(isOpen);
            }));
        }

        private void OnIsOpenChanged(bool isOpen)
        {
            Debug.WriteLine($"DialogHost.IsOpen changed {isOpen}");

            if (isOpen)
            {
                Debug.Assert(window == null);
                Unloaded += OnUnloaded;

                window = new()
                {
                    Owner = Window.GetWindow(AttributeParent),
                    WindowStartupLocation = WindowStartupLocation.CenterOwner,
                    WindowStyle = WindowStyle.ToolWindow,
                };

                SetOpener(window, this);
                window.SetBinding(StyleProperty, BindingOn(this, nameof(Style)));
                window.SetBinding(HeightProperty, BindingOn(this, nameof(Height)));
                window.SetBinding(WidthProperty, BindingOn(this, nameof(Width)));
                window.SetBinding(ContentControl.ContentProperty, BindingOn(this, nameof(Content)));
                window.SetBinding(ContentControl.ContentTemplateProperty, BindingOn(this, nameof(ContentTemplate)));

                window.Closing += OnWindowClosing;
                window.Closed += OnWindowClosed;

                Opening?.Invoke(this, EventArgs.Empty);
                window.ShowDialog();
            }
            else
            {
                Debug.Assert(window != null);
                Unloaded -= OnUnloaded;

                window.Closing -= OnWindowClosing;
                window.Hide();
                window.Close();

                window.Closed -= OnWindowClosed;
                window = null;
            }
        }

        //private void RequestClose()
        //{
        //    var command = Command;
        //    var parameter = (object)false;
        //    if (command.CanExecute(parameter))
        //    {
        //        command.Execute(parameter);
        //    }
        //}

        // To `d.prop`
        private static Binding BindingOn(DependencyObject d, string prop) => new(prop) { Source = d };
    }
}
