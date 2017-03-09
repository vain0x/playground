using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading;
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
        sealed class Main
        {
            readonly Timer timer;
            int id;

            public ObservableCollection<ObservableSequence<int>> Lists { get; } =
                new ObservableCollection<ObservableSequence<int>>();

            public void Add()
            {
                Lists.Add(new ObservableSequence<int>());
            }

            public Main()
            {
                timer =
                    new Timer(state =>
                    {
                        foreach (var list in Lists)
                        {
                            list.InsertAsync(0, Interlocked.Increment(ref id));
                        }
                    }, null, 1000, 300);
            }
        }

        readonly Main main;

        public MainWindow()
        {
            InitializeComponent();

            main = new Main();
            DataContext = main;
        }

        private void Button_Click(object sender, RoutedEventArgs e)
        {
            main.Add();
        }
    }
}
