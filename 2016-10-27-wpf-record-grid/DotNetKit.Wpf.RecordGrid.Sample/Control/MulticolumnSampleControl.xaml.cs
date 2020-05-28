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

namespace DotNetKit.Wpf.Sample
{
    /// <summary>
    /// MulticolumnSampleControl.xaml の相互作用ロジック
    /// </summary>
    public partial class MulticolumnSampleControl : UserControl
    {
        public MulticolumnSampleControl()
        {
            InitializeComponent();

            DataContext =
                new
                {
                    Name = "Miku",
                    Code = "CV01",
                    Age = 16,
                    Gender = "Female",
                };
        }
    }
}
