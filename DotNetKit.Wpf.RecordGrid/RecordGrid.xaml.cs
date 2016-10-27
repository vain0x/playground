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

        void AddUIElement(int index, RecordItemTemplate template)
        {
            grid.RowDefinitions.Add(new RowDefinition() { Height = GridLength.Auto });

            var labelTemplate = template.LabelTemplate;
            if (labelTemplate != null)
            {
                var label = (UIElement)labelTemplate.LoadContent();
                label.SetValue(Grid.RowProperty, index);
                grid.Children.Add(label);
            }
            
            var valueTemplate = template.ValueTemplate;
            if (valueTemplate != null)
            {
                var value = (UIElement)valueTemplate.LoadContent();
                value.SetValue(Grid.RowProperty, index);
                value.SetValue(Grid.ColumnProperty, 1);
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
            Items = new ObservableCollection<RecordItemTemplate>();
            Items.CollectionChanged += OnCollectionChanged;

            InitializeComponent();
        }
    }
}
