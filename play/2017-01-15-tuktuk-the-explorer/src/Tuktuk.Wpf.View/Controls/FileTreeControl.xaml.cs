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

namespace Tuktuk.Wpf.Controls
{
    /// <summary>
    /// FileTreeControl.xaml の相互作用ロジック
    /// </summary>
    public partial class FileTreeControl : UserControl
    {
        FileTree FileTree
        {
            get { return (FileTree)DataContext; }
            set { DataContext = value; }
        }

        public FileTreeControl()
        {
            InitializeComponent();
        }

        private void treeView_SelectedItemChanged(object sender, RoutedPropertyChangedEventArgs<object> e)
        {
            FileTree.OnSelected(e.NewValue);
        }
    }
}
