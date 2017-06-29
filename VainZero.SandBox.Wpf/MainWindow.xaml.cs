using System;
using System.Collections;
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
    /// MainWindow.xaml の相互作用ロジック
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();

            var iota = Enumerable.Range(0, 100).ToArray();
            var i = 0;
            var nextItems =
                new Func<IEnumerable>(() =>
                {
                    i++;
                    if (i > iota.Length) return new int[0];
                    return new[] { iota[i - 1] };
                });

            DataContext =
                new
                {
                    NextItems = nextItems,
                };
        }
    }
}
