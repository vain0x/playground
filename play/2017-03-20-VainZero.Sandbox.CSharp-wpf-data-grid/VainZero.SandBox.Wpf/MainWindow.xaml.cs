using System;
using System.Collections.Generic;
using System.Diagnostics;
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
using System.ComponentModel;
using System.Runtime.CompilerServices;

namespace VainZero.SandBox.Wpf
{
    /// <summary>
    /// MainWindow.xaml の相互作用ロジック
    /// </summary>
    public partial class MainWindow : Window
    {
        sealed class Log
            : IDisposable
        {
            string Name { get; }

            public void Dispose()
            {
                Debug.WriteLine($"End {Name}");
            }

            public Log([CallerMemberName] string name = default(string))
            {
                Name = name;
                Debug.WriteLine($"Begin {Name}");
            }
        }

        sealed class ImmutablePerson
        {
            public string Name { get; }
            public int Age { get; }

            public ImmutablePerson(string name, int age)
            {
                Name = name;
                Age = age;
            }
        }

        sealed class Person
            : IEditableObject
        {
            public string Name { get; set; }
            public int Age { get; set; }
            public string Memo { get; set; }

            ImmutablePerson ToImmutable()
            {
                return new ImmutablePerson(Name, Age);
            }

            ImmutablePerson backup;

            public void BeginEdit()
            {
                using (new Log())
                {
                    backup = ToImmutable();
                }
            }

            public void CancelEdit()
            {
                using (new Log())
                {
                    Name = backup.Name;
                    Age = backup.Age;
                }
            }

            public void EndEdit()
            {
                using (new Log())
                {
                    backup = ToImmutable();
                }
            }

            public Person()
            {
            }

            public Person(string name, int age)
            {
                Name = name;
                Age = age;
            }
        }

        public MainWindow()
        {
            InitializeComponent();

            var items =
                new BindingList<Person>()
                {
                    new Person("John Due", 23),
                    new Person("Holy Kana", 21),
                };

            DataContext =
                new
                {
                    Items = items,
                };
        }
    }
}
