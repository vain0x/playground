using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Markup;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace DotNetKit.Wpf
{
    /// <summary>
    /// RecordGrid.xaml の相互作用ロジック
    /// </summary>
    [ContentProperty("Items")]
    public partial class RecordGrid : UserControl
    {
        public ObservableCollection<RecordItemTemplate> Items { get; private set; }
        public Orientation Orientation { get; set; }

        DependencyProperty GridRowProperty
        {
            get { return Orientation == Orientation.Vertical ? Grid.RowProperty : Grid.ColumnProperty; }
        }

        DependencyProperty GridColumnProperty
        {
            get { return Orientation == Orientation.Vertical ? Grid.ColumnProperty : Grid.RowProperty; }
        }

        void AddGridRow()
        {
            if (Orientation == Orientation.Vertical)
            {
                grid.RowDefinitions.Add(new RowDefinition() { Height = GridLength.Auto });
            }
            else
            {
                grid.ColumnDefinitions.Add(new ColumnDefinition() { Width = GridLength.Auto });
            }
        }

        void AddGridColumn(GridLength length)
        {
            if (Orientation == Orientation.Vertical)
            {
                grid.ColumnDefinitions.Add(new ColumnDefinition() { Width = length });
            }
            else
            {
                grid.RowDefinitions.Add(new RowDefinition() { Height = length });
            }
        }

        bool isColumnDefinitionsCreated;

        void CreateColumnDefinitions()
        {
            if (isColumnDefinitionsCreated) return;
            isColumnDefinitionsCreated = true;

            AddGridColumn(GridLength.Auto);
            AddGridColumn(new GridLength(1.0, GridUnitType.Star));
        }

        void AddUIElement(int index, RecordItemTemplate template)
        {
            CreateColumnDefinitions();

            AddGridRow();

            var labelTemplate = template.LabelTemplate;
            if (labelTemplate != null)
            {
                var label = (UIElement)labelTemplate.LoadContent();
                label.SetValue(GridColumnProperty, 0);
                label.SetValue(GridRowProperty, index);
                grid.Children.Add(label);
            }
            
            var valueTemplate = template.ValueTemplate;
            if (valueTemplate != null)
            {
                var value = (UIElement)valueTemplate.LoadContent();
                value.SetValue(GridColumnProperty, 1);
                value.SetValue(GridRowProperty, index);
                grid.Children.Add(value);
            }
        }

        void OnCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    var index = e.NewStartingIndex;
                    foreach (var template in e.NewItems.Cast<RecordItemTemplate>())
                    {
                        AddUIElement(index, template);
                        index++;
                    }
                    break;
            }
        }

        public RecordGrid()
        {
            Orientation = Orientation.Vertical;

            Items = new ObservableCollection<RecordItemTemplate>();
            Items.CollectionChanged += OnCollectionChanged;

            InitializeComponent();
        }
    }
}
