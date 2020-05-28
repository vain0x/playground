using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
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

            DataContext = new ViewModel();
        }
    }

    public class ViewModel
    {
        public ObservableCollection<Person> Persons { get; } =
            new ObservableCollection<Person>()
            {
                new Person(),
                new Person() { Name = "Rin", Age = 14, Priority = 1 },
                new Person() { Name = "Yukari", Age = 18, Priority = 2 },
                new Person() { Name = "Una", Age = 11, Priority = 3 },
            };

        public GongSolutions.Wpf.DragDrop.IDropTarget PersonsDropHandler { get; } =
            new PersonsDropTarget();

        sealed class PersonsDropTarget
            : GongSolutions.Wpf.DragDrop.IDropTarget
        {
            public void DragOver(GongSolutions.Wpf.DragDrop.IDropInfo dropInfo)
            {
                var sourceIndex = dropInfo.DragInfo.SourceIndex;
                var insertIndex = dropInfo.InsertIndex;
                if (sourceIndex == 0 || insertIndex == 0)
                {
                    dropInfo.Effects = DragDropEffects.None;
                    return;
                }

                GongSolutions.Wpf.DragDrop.DragDrop.DefaultDropHandler.DragOver(dropInfo);
            }

            public void Drop(GongSolutions.Wpf.DragDrop.IDropInfo dropInfo)
            {
                GongSolutions.Wpf.DragDrop.DragDrop.DefaultDropHandler.Drop(dropInfo);

                var sourceIndex = dropInfo.DragInfo.SourceIndex;
                var insertIndex = dropInfo.InsertIndex;
                if (sourceIndex == insertIndex || sourceIndex == insertIndex - 1) return;

                var persons = (ObservableCollection<Person>)dropInfo.TargetCollection;
                if (persons.Count <= 1) return;

                var index = insertIndex - (sourceIndex < insertIndex ? 1 : 0);
                var person = persons[index];
                person.Priority =
                    index == 0
                        ? persons[1].Priority - 1 :
                    index == persons.Count - 1
                        ? persons[persons.Count - 2].Priority + 1 :
                    (persons[index - 1].Priority + persons[index + 1].Priority) / 2;

                Console.WriteLine(string.Join(", ", persons.OrderBy(p => p.Priority)));
            }
        }
    }

    public class Person
    {
        public string Name { get; set; } = "Miku";
        public int Age { get; set; } = 16;
        public double Priority { get; set; }

        public override string ToString()
        {
            return string.Concat(Name, "(", Age, ")");
        }
    }
}
