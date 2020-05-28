using System;
using System.Collections;
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
using System.Windows.Navigation;
using System.Windows.Shapes;
using Reactive.Bindings;

namespace VainZero.SandBox.Wpf
{
    public sealed class Behavior<TSource, TTarget>
        : INotifyPropertyChanged
        , INotifyDataErrorInfo
    {
        public event EventHandler<DataErrorsChangedEventArgs> ErrorsChanged;

        public event PropertyChangedEventHandler PropertyChanged;

        public bool HasErrors
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        public IEnumerable GetErrors(string propertyName)
        {
            throw new NotImplementedException();
        }
    }

    public sealed class Person
    {
        public string Name { get; }
        public ReactiveProperty<int> Age { get; }

        public Person(string name, int age)
        {
            Name = name;
            Age = new ReactiveProperty<int>(age);
        }
    }

    public sealed class ViewModel
    {
        public ReactiveProperty<string> Text { get; } =
            new ReactiveProperty<string>("hello");

        public IReadOnlyList<Person> Perons { get; } =

    }

    /// <summary>
    /// MainWindow.xaml の相互作用ロジック
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
        }
    }
}
