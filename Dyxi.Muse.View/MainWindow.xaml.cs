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
using Dyxi.CSharp.Util;

namespace Dyxi.Muse.View
{
    /// <summary>
    /// MainWindow.xaml の相互作用ロジック
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();

            this.DataContext = this._vm = new ViewModel.MainWindow();

            this._musicColl.DataContext = Muse.Model.MusicColl.Instance;

            this._settingsWindow = new SettingsWindow(this._vm.SettingsWindow);
        }

        private void _collTree_SelectedItemChanged(object sender, RoutedPropertyChangedEventArgs<object> e)
        {
            var item = e.NewValue as TreeViewItem;
            var coll = (item != null ? item.DataContext as Model.IColl : null);
            if (coll == null) return;
            _vm.Coll = coll;
        }

        private void _listView_MouseDoubleClick(object sender, MouseButtonEventArgs e)
        {
            var media = ((FrameworkElement)e.OriginalSource).DataContext as Model.media;
            if (media == null) return;
            // TODO: スタックに media を積む
        }

        private void Window_Closed(object sender, EventArgs e)
        {
            _vm.SettingsWindow.Hide();
            _settingsWindow.Close();
        }

        private ViewModel.MainWindow _vm;
        private SettingsWindow _settingsWindow;
    }
}
