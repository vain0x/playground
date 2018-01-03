using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
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
using System.Windows.Controls.Primitives;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;
using Prism.Commands;
using Prism.Mvvm;
using Reactive.Bindings;

namespace VainZero.Sandbox.Wpf
{
    /// <summary>
    /// MainWindow.xaml の相互作用ロジック
    /// </summary>
    public partial class MainWindow : Window
    {
        static MainWindow()
        {
            var dateTimeFormat = new DateTimeFormatInfo();
            dateTimeFormat.LongDatePattern = "yyyy年M月d日(ddd)";
            dateTimeFormat.ShortDatePattern = "yyyy/M/d";

            var culture = new CultureInfo("ja-jp");
            culture.DateTimeFormat = dateTimeFormat;
            //CultureInfo.CurrentCulture = culture;
            //CultureInfo.CurrentUICulture = culture;
        }

        public MainWindow()
        {
            InitializeComponent();

            var notify = Pulse.Lazy(() =>
            {
                Debug.WriteLine("Fire.");
                return default(Unit);
            });

            FireCommand.Subscribe(async _ =>
            {
                var x = await notify;
                Debug.WriteLine(x);
            });

            DataContext = this;
        }

        public ReactiveCommand FireCommand { get; } =
            new ReactiveCommand();

        public ReactiveProperty<DateTime?> Date { get; } =
            new ReactiveProperty<DateTime?>(DateTime.Now.Date);

        public ReactiveProperty<DatePickerFormat> DatePickerFormat { get; } =
            new ReactiveProperty<DatePickerFormat>(System.Windows.Controls.DatePickerFormat.Short);

        static IEnumerable<DependencyObject> Descendants(DependencyObject root)
        {
            var stack = new Stack<DependencyObject>();
            stack.Push(root);

            while (stack.Count > 0)
            {
                var d = stack.Pop();
                yield return d;

                var count = VisualTreeHelper.GetChildrenCount(d);
                for (var i = 0; i < count; i++)
                {
                    stack.Push(VisualTreeHelper.GetChild(d, count - i - 1));
                }
            }
        }

        static void Regularize(DatePicker datePicker, TextBox textBox)
        {
            if (datePicker.IsKeyboardFocusWithin)
            {
                textBox.Text = datePicker.SelectedDate.HasValue ? datePicker.SelectedDate.Value.ToString("yyyy/MM/dd") : "";
            }
            else
            {
                textBox.Text = datePicker.SelectedDate.HasValue ? datePicker.SelectedDate.Value.ToString("yyyy年M月d日") : "";
            }
        }

        void Attach(DatePicker datePicker, TextBox textBox)
        {
            /*
            datePicker.SelectedDateChanged += (sender, e) =>
            {
                Regularize(datePicker, textBox);
            };

            datePicker.IsKeyboardFocusWithinChanged += (sender, e) =>
            {
                Regularize(datePicker, textBox);
            };
            */

            datePicker.IsKeyboardFocusWithinChanged += (sender, e) =>
            {
                datePicker.SelectedDateFormat = (bool)e.NewValue ? System.Windows.Controls.DatePickerFormat.Short : System.Windows.Controls.DatePickerFormat.Long;
            };

            datePicker.SetBinding(DatePicker.SelectedDateFormatProperty, new Binding("DatePickerFormat.Value") { Source = this });
        }

        private void DatePicker_Loaded(object sender, RoutedEventArgs e)
        {
            var datePicker = (DatePicker)sender;
            var textBox = Descendants(datePicker).OfType<TextBox>().First();
            Attach(datePicker, textBox);
        }
    }

    public static class AsyncSubjectExtension
    {
        public static void Resolve<X>(this AsyncSubject<X> @this, X value)
        {
            @this.OnNext(value);
            @this.OnCompleted();
        }

        public static void Resolve(this AsyncSubject<Unit> @this)
        {
            @this.OnNext(default(Unit));
            @this.OnCompleted();
        }

        public static void WaitForTermination<X>(this IObservable<X> @this)
        {
            @this.Materialize().Wait();
        }
    }

    public static class Pulse
    {
        public static IObservable<X> Run<X>(Func<X> func)
        {
            return Observable.Create<X>(observer =>
            {
                X result;
                try
                {
                    result = func();
                }
                catch (Exception ex)
                {
                    observer.OnError(ex);
                    return Disposable.Empty;
                }
                observer.OnNext(result);
                observer.OnCompleted();
                return Disposable.Empty;
            });
        }

        public static IObservable<X> Lazy<X>(Func<X> func)
        {
            var lazy = new Lazy<X>(func);
            return Run(() => lazy.Value);
        }
    }
}
