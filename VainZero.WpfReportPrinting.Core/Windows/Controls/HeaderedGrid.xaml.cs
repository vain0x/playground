using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Collections.Specialized;
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

namespace VainZero.Windows.Controls
{
    /// <summary>
    /// HeaderedGrid.xaml の相互作用ロジック
    /// </summary>
    public partial class HeaderedGrid : UserControl
    {
        public ScrollViewer ScrollViewer => scrollViewer;
        public Grid Grid => grid;

        public ObservableCollection<HeaderedGridColumn> Columns { get; } =
            new ObservableCollection<HeaderedGridColumn>();

        #region ItemsSource
        public static DependencyProperty ItemsSourceProperty { get; } =
            DependencyProperty.Register(
                nameof(ItemsSource),
                typeof(IEnumerable),
                typeof(HeaderedGrid),
                new FrameworkPropertyMetadata()
                {
                    PropertyChangedCallback = OnItemsSourceChanged,
                }
            );

        public IEnumerable ItemsSource
        {
            get { return (IEnumerable)GetValue(ItemsSourceProperty); }
            set { SetValue(ItemsSourceProperty, value); }
        }

        static void OnItemsSourceChanged(DependencyObject sender, DependencyPropertyChangedEventArgs e)
        {
            var @this = (HeaderedGrid)sender;
            @this.Reset();
        }
        #endregion

        void AddColumnDefinition(HeaderedGridColumn column)
        {
            var columnDefinition =
                new ColumnDefinition()
                {
                    Width = column.Width,
                };
            grid.ColumnDefinitions.Add(columnDefinition);
        }

        void AddHeaderCell(int columnIndex)
        {
            var column = Columns[columnIndex];
            var cell =
                new HeaderedGridHeaderCell()
                {
                    Content = column.Header,
                    ContentTemplate = column.HeaderTemplate,
                    ContentTemplateSelector = column.HeaderTemplateSelector,
                };
            Grid.SetRow(cell, 0);
            Grid.SetColumn(cell, columnIndex);
            grid.Children.Add(cell);
        }

        void AddRowCell(int rowIndex, int columnIndex, object dataContext)
        {
            var column = Columns[columnIndex];
            var cell =
                new HeaderedGridRowCell()
                {
                    Content = dataContext,
                    ContentTemplate = column.CellTemplate,
                    ContentTemplateSelector = column.CellTemplateSelector,
                };
            Grid.SetRow(cell, rowIndex);
            Grid.SetColumn(cell, columnIndex);
            grid.Children.Add(cell);
        }

        void Reset()
        {
            grid.Children.Clear();
            grid.ColumnDefinitions.Clear();
            if (grid.RowDefinitions.Count > 1)
            {
                grid.RowDefinitions.RemoveRange(1, grid.RowDefinitions.Count - 1);
            }

            foreach (var columnIndex in Enumerable.Range(0, Columns.Count))
            {
                AddColumnDefinition(Columns[columnIndex]);
                AddHeaderCell(columnIndex);
            }

            var items = ItemsSource;
            if (items != null)
            {
                var rowIndex = 1;
                foreach (var item in items)
                {
                    grid.RowDefinitions.Add(new RowDefinition() { Height = GridLength.Auto });

                    foreach (var columnIndex in Enumerable.Range(0, Columns.Count))
                    {
                        AddRowCell(rowIndex, columnIndex, item);
                    }

                    rowIndex++;
                }
            }
        }

        void OnColumnCollectionChanged(object sender, NotifyCollectionChangedEventArgs e)
        {
            if (e.Action != NotifyCollectionChangedAction.Add)
            {
                throw new NotSupportedException($"{nameof(HeaderedGrid)}.{nameof(Columns)} doesn't support changes except for adding.");
            }

            var columnIndex = e.NewStartingIndex;
            foreach (var column in e.NewItems.Cast<HeaderedGridColumn>())
            {
                AddColumnDefinition(column);
                AddHeaderCell(columnIndex);

                var items = ItemsSource;
                if (items != null)
                {
                    var rowIndex = 1;
                    foreach (var item in items)
                    {
                        AddRowCell(rowIndex, columnIndex, item);
                        rowIndex++;
                    }
                }
                columnIndex++;
            }
        }

        public HeaderedGrid()
        {
            InitializeComponent();

            Columns.CollectionChanged += OnColumnCollectionChanged;
        }
    }
}
