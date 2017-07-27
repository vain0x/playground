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
    /// TaskControl.xaml の相互作用ロジック
    /// </summary>
    public partial class TaskControl : UserControl
    {
        public TaskControl()
        {
            InitializeComponent();
        }

        public sealed class Status
        {
            public Visibility ContentVisibility { get; set; }
            public object Content { get; set; }
            public DataTemplate ContentDataTemplate { get; set; }
            public DataTemplateSelector ContentDataTemplateSelector { get; set; }

            public Visibility BusyIndicatorVisibility { get; set; }
            public ICommand CancelCommand { get; set; }
        }
    }
}
