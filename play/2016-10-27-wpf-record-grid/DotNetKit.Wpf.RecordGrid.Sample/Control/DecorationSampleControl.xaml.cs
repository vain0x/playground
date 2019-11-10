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
    /// DecorationSampleControl.xaml の相互作用ロジック
    /// </summary>
    public partial class DecorationSampleControl : UserControl
    {
        public class Person
        {
            public string Name { get; set; }
            public int Age { get; set; }
            public string Gender { get; set; }
            public string Birthday { get; set; }
            public string Masterpiece { get; set; }
        }

        public class Singers
        {
            public Person Miku { get; set; }
        }

        public DecorationSampleControl()
        {
            InitializeComponent();

            DataContext =
                new Person()
                {
                    Name = "Miku",
                    Age = 16,
                    Gender = "Female",
                    Birthday = "08/31",
                    Masterpiece = "Ghost Rule",
                };
        }
    }
}
