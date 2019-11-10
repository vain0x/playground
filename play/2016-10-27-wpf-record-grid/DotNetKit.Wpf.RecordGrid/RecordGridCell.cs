using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;

namespace DotNetKit.Wpf
{
    public sealed class RecordGridCell
        : Border
    {
        #region IsLabel
        static readonly DependencyProperty isLabelProperty =
            DependencyProperty.Register(
                "IsLabel",
                typeof(bool),
                typeof(RecordGridCell)
            );

        public static DependencyProperty IsLabelProperty
        {
            get { return isLabelProperty; }
        }

        public bool IsLabel
        {
            get { return (bool)GetValue(IsLabelProperty); }
            set { SetValue(IsLabelProperty, value); }
        }
        #endregion

        #region IsOdd
        static readonly DependencyProperty isOddProperty =
            DependencyProperty.Register(
                "IsOdd",
                typeof(bool),
                typeof(RecordGridCell)
            );

        public static DependencyProperty IsOddProperty
        {
            get { return isOddProperty; }
        }

        public bool IsOdd
        {
            get { return (bool)GetValue(IsOddProperty); }
            set { SetValue(IsOddProperty, value); }
        }
        #endregion
    }
}
