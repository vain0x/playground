using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;

namespace VainZero.Windows.Controls
{
    public class HeaderedGridColumn
        : DependencyObject
    {
        #region Width
        public static DependencyProperty WidthProperty { get; } =
            DependencyProperty.Register(
                nameof(Width),
                typeof(GridLength),
                typeof(HeaderedGridColumn)
            );

        public GridLength Width
        {
            get { return (GridLength)GetValue(WidthProperty); }
            set { SetValue(WidthProperty, value); }
        }
        #endregion

        #region Header
        public static DependencyProperty HeaderProperty { get; } =
            DependencyProperty.Register(
                nameof(Header),
                typeof(object),
                typeof(HeaderedGridColumn)
            );

        public object Header
        {
            get { return GetValue(HeaderProperty); }
            set { SetValue(HeaderProperty, value); }
        }
        #endregion

        #region HeaderTemplate
        public static DependencyProperty HeaderTemplateProperty { get; } =
            DependencyProperty.Register(
                nameof(HeaderTemplate),
                typeof(DataTemplate),
                typeof(HeaderedGridColumn)
            );

        public DataTemplate HeaderTemplate
        {
            get { return (DataTemplate)GetValue(HeaderTemplateProperty); }
            set { SetValue(HeaderTemplateProperty, value); }
        }
        #endregion

        #region HeaderTemplateSelector
        public static DependencyProperty HeaderTemplateSelectorProperty { get; } =
            DependencyProperty.Register(
                nameof(HeaderTemplateSelector),
                typeof(DataTemplateSelector),
                typeof(HeaderedGridColumn)
            );

        public DataTemplateSelector HeaderTemplateSelector
        {
            get { return (DataTemplateSelector)GetValue(HeaderTemplateSelectorProperty); }
            set { SetValue(HeaderTemplateSelectorProperty, value); }
        }
        #endregion

        #region CellTemplate
        public static DependencyProperty CellTemplateProperty { get; } =
            DependencyProperty.Register(
                nameof(CellTemplate),
                typeof(DataTemplate),
                typeof(HeaderedGridColumn)
            );

        public DataTemplate CellTemplate
        {
            get { return (DataTemplate)GetValue(CellTemplateProperty); }
            set { SetValue(CellTemplateProperty, value); }
        }
        #endregion

        #region CellTemplateSelector
        public static DependencyProperty CellTemplateSelectorProperty { get; } =
            DependencyProperty.Register(
                nameof(CellTemplateSelector),
                typeof(DataTemplateSelector),
                typeof(HeaderedGridColumn)
            );

        public DataTemplateSelector CellTemplateSelector
        {
            get { return (DataTemplateSelector)GetValue(CellTemplateSelectorProperty); }
            set { SetValue(CellTemplateSelectorProperty, value); }
        }
        #endregion

        #region CellBinding
        internal static BindingBase CellBindingDefault { get; } =
            new Binding();

        public BindingBase CellBinding { get; set; } =
            CellBindingDefault;
        #endregion

        public Style CellStyle { get; set; }
    }
}
