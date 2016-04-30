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
            
            this._musicColl.DataContext = Model.Types.Coll.MusicColl;

            this._settingsDialog = new SettingsDialog(this._vm.SettingsDialog);
        }

        private void _collTree_SelectedItemChanged(object sender, RoutedPropertyChangedEventArgs<object> e)
        {
            var item = e.NewValue as TreeViewItem;
            var coll = (item != null ? item.DataContext as Model.Types.Coll : null);
            if (coll == null) return;
            this._vm.Coll = coll;
        }

        private void _listView_MouseDoubleClick(object sender, MouseButtonEventArgs e)
        {
            var item = ((FrameworkElement)e.OriginalSource).DataContext as Model.Types.Coll;
            if (item == null) return;
        }

        private ViewModel.MainWindow _vm;
        private SettingsDialog _settingsDialog;
    }
}
