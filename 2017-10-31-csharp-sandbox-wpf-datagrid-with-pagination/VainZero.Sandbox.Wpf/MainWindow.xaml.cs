using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reactive.Concurrency;
using System.Reactive.Disposables;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.Reactive.Threading.Tasks;
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
using Prism.Commands;
using Prism.Mvvm;
using Reactive.Bindings;

namespace VainZero.Sandbox.Wpf
{
    /// <summary>
    /// MainWindow.xaml の相互作用ロジック
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();

            var sortCommand = new ReactiveCommand<DataGridColumn>();
            sortCommand.Subscribe(e =>
            {
                e.SortDirection = e.SortDirection == System.ComponentModel.ListSortDirection.Ascending ? System.ComponentModel.ListSortDirection.Descending : System.ComponentModel.ListSortDirection.Ascending;
            });

            DataContext =
                new
                {
                    SortCommand = sortCommand,
                    Items =
                        Enumerable.Range(0, 10)
                        .Select(i => new { Id = i, Name = $"Person {i + 1}" })
                        .ToArray(),
                };
        }
    }

    public static class DataGridPaginationExtension
    {
        #region SortCommand
        public static readonly DependencyProperty SortCommandProperty =
            DependencyProperty.RegisterAttached(
                "SortCommand",
                typeof(ICommand),
                typeof(DataGridPaginationExtension),
                new PropertyMetadata()
                {
                    PropertyChangedCallback = OnSortCommandChanged,
                });

        public static ICommand GetSortCommand(DependencyObject obj)
        {
            return (ICommand)obj.GetValue(SortCommandProperty);
        }

        public static void SetSortCommand(DependencyObject obj, ICommand value)
        {
            obj.SetValue(SortCommandProperty, value);
        }

        private static void OnDataGridSorting(object sender, DataGridSortingEventArgs e)
        {
            var dataGrid = (DataGrid)sender;
            var sortCommand = GetSortCommand(dataGrid);
            if (sortCommand != null && sortCommand.CanExecute(e))
            {
                e.Handled = true;
                sortCommand.Execute(e.Column);
            }
        }

        private static void OnSortCommandChanged(DependencyObject obj, DependencyPropertyChangedEventArgs e)
        {
            var dataGrid = (DataGrid)obj;
            var oldCommand = e.OldValue as ICommand;
            var newCommand = e.NewValue as ICommand;

            if (oldCommand != null)
            {
                dataGrid.Sorting -= OnDataGridSorting;
            }
            else if (newCommand != null)
            {
                dataGrid.Sorting += OnDataGridSorting;
            }
        }
        #endregion
    }
}
