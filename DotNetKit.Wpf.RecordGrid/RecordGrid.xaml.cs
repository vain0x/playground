using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Linq;
using System.Text;
using System.Threading;
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
    [ContentProperty("Children")]
    public partial class RecordGrid
        : UserControl
    {
        public Orientation Orientation { get; set; }
        public ObservableCollection<UIElement> Children { get; private set; }

        #region IsLabel
        static readonly DependencyProperty isLabelProperty =
            DependencyProperty.RegisterAttached(
                "IsLabel",
                typeof(bool),
                typeof(RecordGrid)
            );

        public static DependencyProperty IsLabelProperty
        {
            get { return isLabelProperty; }
        }

        public static bool GetIsLabel(DependencyObject obj)
        {
            return (bool)obj.GetValue(IsLabelProperty);
        }

        public static void SetIsLabel(DependencyObject obj, bool value)
        {
            obj.SetValue(IsLabelProperty, value);
        }
        #endregion

        #region IsOdd
        static readonly DependencyProperty isOddProperty =
            DependencyProperty.RegisterAttached(
                "IsOdd",
                typeof(bool),
                typeof(RecordGrid)
            );

        public static DependencyProperty IsOddProperty
        {
            get { return isOddProperty; }
        }

        public static bool GetIsOdd(DependencyObject obj)
        {
            return (bool)obj.GetValue(IsOddProperty);
        }

        public static void SetIsOdd(DependencyObject obj, bool value)
        {
            obj.SetValue(IsOddProperty, value);
        }
        #endregion

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

        void AddUIElement(int index, bool isLabel, UIElement element)
        {
            var isOdd = index % 2 != 0;

            CreateColumnDefinitions();

            if (isLabel)
            {
                AddGridRow();
            }

            element.SetValue(GridRowProperty, index);
            element.SetValue(GridColumnProperty, isLabel ? 0 : 1);
            SetIsLabel(element, isLabel);
            SetIsOdd(element, isOdd);
            grid.Children.Add(element);
        }

        void OnCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            switch (e.Action)
            {
                case NotifyCollectionChangedAction.Add:
                    var index = e.NewStartingIndex;
                    foreach (var element in e.NewItems.Cast<UIElement>())
                    {
                        AddUIElement(index / 2, index % 2 == 0, element);
                        index++;
                    }
                    break;
            }
        }

        public RecordGrid()
        {
            Orientation = Orientation.Vertical;
            Children = new ObservableCollection<UIElement>();
            Children.CollectionChanged += OnCollectionChanged;

            InitializeComponent();
        }
    }
}
