using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
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

namespace DotNetKit.Wpf
{
    [ContentProperty("ValueTemplate")]
    public partial class RecordItemTemplate : DependencyObject
    {
        #region LabelTemplate
        public event DependencyPropertyChangedEventHandler LabelTemplateChanged;

        void RaiseLabelTemplateChanged(DependencyPropertyChangedEventArgs e)
        {
            var h = LabelTemplateChanged;
            if (h != null) h.Invoke(this, e);
        }

        public static void OnLabelTemplateChanged(DependencyObject sender, DependencyPropertyChangedEventArgs e)
        {
            ((RecordItemTemplate)sender).RaiseLabelTemplateChanged(e);
        }

        static readonly DependencyProperty labelTemplateProperty =
            DependencyProperty.Register(
                "LabelTemplate",
                typeof(DataTemplate),
                typeof(RecordGrid),
                new PropertyMetadata(OnLabelTemplateChanged)
            );

        public static DependencyProperty LabelTemplateProperty
        {
            get { return labelTemplateProperty; }
        }

        public DataTemplate LabelTemplate
        {
            get { return (DataTemplate)GetValue(LabelTemplateProperty); }
            set { SetValue(LabelTemplateProperty, value); }
        }
        #endregion

        #region ValueTemplate
        public event DependencyPropertyChangedEventHandler ValueTemplateChanged;

        void RaiseValueTemplateChanged(DependencyPropertyChangedEventArgs e)
        {
            var h = ValueTemplateChanged;
            if (h != null) h.Invoke(this, e);
        }

        public static void OnValueTemplateChanged(DependencyObject sender, DependencyPropertyChangedEventArgs e)
        {
            ((RecordItemTemplate)sender).RaiseValueTemplateChanged(e);
        }

        static readonly DependencyProperty valueTemplateProperty =
            DependencyProperty.Register(
                "ValueTemplate",
                typeof(DataTemplate),
                typeof(RecordGrid),
                new PropertyMetadata(OnValueTemplateChanged)
            );

        public static DependencyProperty ValueTemplateProperty
        {
            get { return valueTemplateProperty; }
        }

        public DataTemplate ValueTemplate
        {
            get { return (DataTemplate)GetValue(ValueTemplateProperty); }
            set { SetValue(ValueTemplateProperty, value); }
        }
        #endregion

        #region IsOdd
        static readonly DependencyProperty isOddProperty =
            DependencyProperty.RegisterAttached(
                "IsOdd",
                typeof(bool),
                typeof(RecordItemTemplate),
                new PropertyMetadata(false)
            );

        public static DependencyProperty IsOddProperty
        {
            get { return isOddProperty; }
        }

        public static bool GetIsOdd(DependencyObject obj)
        {
            return (bool)obj.GetValue(IsOddProperty);
        }

        public static void SetIsOdd(DependencyObject obj, bool value)
        {
            obj.SetValue(IsOddProperty, value);
        }
        #endregion
    }
}
