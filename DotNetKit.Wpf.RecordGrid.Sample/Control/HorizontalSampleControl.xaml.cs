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
    /// HorizontalSampleControl.xaml の相互作用ロジック
    /// </summary>
    public partial class HorizontalSampleControl : UserControl
    {
        public sealed class Person
        {
            public string Name { get; set; }
            public int Age { get; set; }
        }

        public HorizontalSampleControl()
        {
            InitializeComponent();

            DataContext = new Person() { Name = "Yukari", Age = 18 };
        }
    }
}
