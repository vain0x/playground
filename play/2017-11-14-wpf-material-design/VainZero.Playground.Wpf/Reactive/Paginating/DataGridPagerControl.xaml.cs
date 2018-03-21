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

namespace DotNetKit.Paginating
{
    /// <summary>
    /// DataGridPagerControl.xaml の相互作用ロジック
    /// </summary>
    public partial class DataGridPagerControl : UserControl
    {
        public DataGridPagerControl()
        {
            InitializeComponent();
        }

        #region Paginator
        /// <summary>
        /// ページネーターに関する状態を保持するオブジェクトを持つプロパティー。
        /// </summary>
        public static readonly DependencyProperty PaginatorProperty =
            DependencyProperty.Register(
                "Paginator",
                typeof(IPaginator),
                typeof(DataGridPagerControl)
            );

        public IPaginator Paginator
        {
            get { return (IPaginator)GetValue(PaginatorProperty); }
            set { SetValue(PaginatorProperty, value); }
        }
        #endregion

        #region ButtonStyle
        /// <summary>
        /// ページ遷移を要求するボタンのスタイルを持つプロパティー。
        /// </summary>
        public static readonly DependencyProperty ButtonStyleProperty =
            DependencyProperty.Register(
                "ButtonStyle",
                typeof(Style),
                typeof(DataGridPagerControl)
            );

        public Style ButtonStyle
        {
            get { return (Style)GetValue(ButtonStyleProperty); }
            set { SetValue(ButtonStyleProperty, value); }
        }
        #endregion
    }
}
