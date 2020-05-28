using System;
using System.Collections.Generic;
using System.ComponentModel;
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
using System.Windows.Shapes;

namespace Dyxi.Muse.View
{
    /// <summary>
    /// SettingsWindow.xaml の相互作用ロジック
    /// </summary>
    public partial class SettingsWindow : Window
    {
        public SettingsWindow(ViewModel.SettingsWindow vm)
        {
            InitializeComponent();

            this.DataContext = _vm = vm;
        }

        public void Window_Closing(object sender, CancelEventArgs e)
        {
            _vm.OnClosing(e);
        }

        private ViewModel.SettingsWindow _vm;
    }
}
