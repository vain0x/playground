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
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace VainZero.SandBox.Wpf
{
    /// <summary>
    /// BusyContentControl.xaml の相互作用ロジック
    /// </summary>
    public partial class BusyContentControl : ContentControl
    {
        #region IsBusy
        public static readonly DependencyProperty IsBusyProperty =
            DependencyProperty.Register(
                "IsBusy",
                typeof(bool),
                typeof(BusyContentControl)
            );

        public bool IsBusy
        {
            get { return (bool)GetValue(IsBusyProperty); }
            set { SetValue(IsBusyProperty, value); }
        }
        #endregion

        public BusyContentControl()
        {
            InitializeComponent();
        }

        public static readonly IValueConverter IsBusyToVisibilityConverter =
            new BooleanToVisibilityConverter();
    }
}
