using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Collections.ObjectModel;
using System.ComponentModel;
using System.Diagnostics;
using System.Linq;
using System.Reactive.Disposables;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using DotNetKit.Reactive.Collections;
using Optional;
using Optional.Collections;
using Optional.Unsafe;

namespace DotNetKit.Paginating
{
    /// <summary>
    /// <see cref="DataGrid"/> への添付プロパティを提供する。
    /// </summary>
    public static class DataGridPaginationExtension
    {
        #region Paginator
        public static readonly DependencyProperty PaginatorProperty =
            DependencyProperty.RegisterAttached(
                "Paginator",
                typeof(IPaginator),
                typeof(DataGridPaginationExtension),
                new PropertyMetadata()
                {
                    PropertyChangedCallback = OnPaginatorChanged,
                });

        public static IPaginator GetPaginator(DependencyObject obj)
        {
            return (IPaginator)obj.GetValue(PaginatorProperty);
        }

        public static void SetPaginator(DependencyObject obj, IPaginator value)
        {
            obj.SetValue(PaginatorProperty, value);
        }

        private static void OnDataGridSorting(object sender, DataGridSortingEventArgs e)
        {
            var dataGrid = (DataGrid)sender;
            var paginator = GetPaginator(dataGrid);
            if (paginator == null) return;

            // 既定のソート処理を無効化する。
            e.Handled = true;

            // Shift キーが押されていなければ他のカラムのソート条件をリセットする。
            var reset = (Keyboard.Modifiers & ModifierKeys.Shift) == ModifierKeys.None;

            var sortKey = GetSortKey(e.Column);

            paginator.Sort(sortKey, reset);
        }

        /// <summary>
        /// 現在のソート条件を、 <see cref="DataGrid"/> の各カラムのソート表示に反映する。
        /// </summary>
        private static void UpdateSortDirections(IPaginationResult result, DataGrid dataGrid)
        {
            // TODO: SortKey が重複していたら例外が投げられるので対処。
            var sortKeyFromColumn =
                result.PropertySorts.ToDictionary(ps => ps.Key);

            foreach (var column in dataGrid.Columns)
            {
                column.SortDirection =
                    sortKeyFromColumn
                    .GetValueOrNone(GetSortKey(column))
                    .Map(sk => sk.Direction)
                    .ToNullable();
            }
        }

        private static void UpdateByPaginator(DataGrid dataGrid)
        {
            var paginator = GetPaginator(dataGrid) as IPaginator;
            if (paginator == null) return;

            var itemList = dataGrid.ItemsSource as IReactiveArray;
            if (itemList == null) return;

            var result = paginator.Current;
            itemList.Set(result.Items);
            UpdateSortDirections(result, dataGrid);
        }

        private static void AttachPaginator(IPaginator paginator, DataGrid dataGrid)
        {
            var subscription = new SingleAssignmentDisposable();
            SetPaginatorSubscription(dataGrid, subscription);

            var onPropertyChanged =
                new PropertyChangedEventHandler((sender, e) => UpdateByPaginator(dataGrid));

            dataGrid.ItemsSource = paginator.CreateReactiveList();
            dataGrid.Sorting += OnDataGridSorting;
            paginator.PropertyChanged += onPropertyChanged;

            subscription.Disposable = Disposable.Create(() =>
            {
                dataGrid.ItemsSource = Array.Empty<object>();
                dataGrid.Sorting -= OnDataGridSorting;
                paginator.PropertyChanged -= onPropertyChanged;
            });

            UpdateByPaginator(dataGrid);
        }

        private static void DetachPaginator(IPaginator paginator, DataGrid dataGrid)
        {
            SetPaginatorSubscription(dataGrid, Disposable.Empty);
        }

        private static void OnPaginatorChanged(DependencyObject obj, DependencyPropertyChangedEventArgs e)
        {
            var dataGrid = (DataGrid)obj;

            var oldPaginator = e.OldValue as IPaginator;
            if (oldPaginator != null) DetachPaginator(oldPaginator, dataGrid);

            var newPaginator = e.NewValue as IPaginator;
            if (newPaginator != null) AttachPaginator(newPaginator, dataGrid);
        }
        #endregion

        #region PaginatorSubscription
        /// <summary>
        /// <see cref="DataGrid"/> が <see cref="IPaginator"/> に接続したときに確保したリソースを持つプロパティー。
        /// </summary>
        public static readonly DependencyProperty PaginatorSubscriptionProperty =
            DependencyProperty.RegisterAttached(
                "PaginatorSubscription",
                typeof(IDisposable),
                typeof(DataGridPaginationExtension),
                new PropertyMetadata(null)
                {
                    PropertyChangedCallback = OnPaginatorSubscriptionChanged,
                });

        public static IDisposable GetPaginatorSubscription(DependencyObject obj)
        {
            return (IDisposable)obj.GetValue(PaginatorSubscriptionProperty);
        }

        public static void SetPaginatorSubscription(DependencyObject obj, IDisposable value)
        {
            obj.SetValue(PaginatorSubscriptionProperty, value);
        }

        private static void OnPaginatorSubscriptionChanged(DependencyObject obj, DependencyPropertyChangedEventArgs e)
        {
            var dataGrid = (DataGrid)obj;
            var oldDisposable = e.OldValue as IDisposable;
            if (oldDisposable != null)
            {
                oldDisposable.Dispose();
            }
        }
        #endregion

        #region SortKey
        /// <summary>
        /// カラムヘッダーがクリックされてソートが起こるときに、どのプロパティーについてソートすればよいかを判断するための基準となるオブジェクトを持つプロパティー。
        /// </summary>
        public static readonly DependencyProperty SortKeyProperty =
            DependencyProperty.RegisterAttached(
                "SortKey",
                typeof(object),
                typeof(DataGridPaginationExtension)
            );

        public static object GetSortKey(DependencyObject obj)
        {
            return obj.GetValue(SortKeyProperty);
        }

        public static void SetSortKey(DependencyObject obj, object value)
        {
            obj.SetValue(SortKeyProperty, value);
        }
        #endregion
    }
}
