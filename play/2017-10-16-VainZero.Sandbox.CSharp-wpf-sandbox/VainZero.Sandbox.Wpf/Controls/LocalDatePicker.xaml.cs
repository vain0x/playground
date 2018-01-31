using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Globalization;
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
using Reactive.Bindings;

namespace VainZero.Sandbox.Wpf.Controls
{
    /// <summary>
    /// LocalDatePicker.xaml の相互作用ロジック
    /// </summary>
    public partial class LocalDatePicker : UserControl
    {
        sealed class DateConverter
            : IValueConverter
        {
            public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
            {
                var date = (DateTime?)value;
                return date.HasValue ? date.Value.ToString("yyyy年M月d日") : "NONE";
            }

            public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
            {
                throw new NotImplementedException();
            }
        }

        public LocalDatePicker()
        {
            InitializeComponent();

            _textBox.SetBinding(TextBox.TextProperty, new Binding("SelectedDate") { Source = _datePicker, Converter = new DateConverter() });

            Content = _textBox;
        }

        TextBox _textBox = new TextBox();
        DatePicker _datePicker = new DatePicker();

        public static readonly DependencyProperty SelectedDateProperty =
            DependencyProperty.Register("SelectedDate", typeof(DateTime?), typeof(LocalDatePicker));

        public DateTime? SelectedDate
        {
            get { return (DateTime?)GetValue(SelectedDateProperty); }
            set { SetValue(SelectedDateProperty, value); }
        }

        protected override void OnIsKeyboardFocusWithinChanged(DependencyPropertyChangedEventArgs e)
        {
            var isKeyboardFocusWithin = (bool)e.NewValue;
            if (isKeyboardFocusWithin)
            {
                Content = _datePicker;
                _datePicker.Focus();
            }
            else
            {
                Content = _textBox;
            }

            base.OnIsKeyboardFocusWithinChanged(e);
        }
    }
}
