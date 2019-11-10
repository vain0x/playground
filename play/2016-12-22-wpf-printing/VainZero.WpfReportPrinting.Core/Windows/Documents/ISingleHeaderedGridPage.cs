using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using VainZero.Windows.Controls;
using VainZero.Windows.Media;

namespace VainZero.Windows.Documents
{
    /// <summary>
    /// ちょうど1つの HeaderedGrid を含むページを表す。
    /// </summary>
    /// <typeparam name="X"></typeparam>
    public interface ISingleHeaderedGridPage<X>
    {
        IReadOnlyList<X> Items { get; }
    }

    public static class SingleHeaderedGridPageExtension
    {
        /// <summary>
        /// ページに含まれる HeaderedGrid をスクロールすることにより、ページネーションを行う。
        /// </summary>
        /// <typeparam name="X"></typeparam>
        /// <param name="this"></param>
        /// <param name="pageSize"></param>
        /// <returns></returns>
        public static IEnumerable<IReadOnlyList<X>>
            Paginate<X>(this ISingleHeaderedGridPage<X> @this, Size pageSize)
        {
            var presenter =
                new ContentPresenter()
                {
                    Content = @this,
                    Width = pageSize.Width,
                    Height = pageSize.Height,
                };

            presenter.Measure(pageSize);
            presenter.Arrange(new Rect(new Point(0, 0), pageSize));
            presenter.UpdateLayout();

            var headeredGrid =
                presenter.VisualDescendantsBreadthFirstOrder().OfType<HeaderedGrid>().First();

            var scrollViewer = headeredGrid.ScrollViewer;
            var grid = headeredGrid.Grid;

            var items = @this.Items;
            var index = 0;
            while (index < items.Count)
            {
                // Count the visible rows.
                var totalRowHeight =
                    grid.RowDefinitions[0].ActualHeight;
                var count = 0;
                while (index + count < items.Count)
                {
                    totalRowHeight += grid.RowDefinitions[1 + index + count].ActualHeight;
                    if (totalRowHeight > scrollViewer.ViewportHeight) break;

                    count++;
                }

                if (count == 0)
                {
                    throw new Exception("Because of too large header row or too small height, the page can't show any rows.");
                }

                yield return
                    Enumerable.Range(index, count).Select(i => items[i]).ToArray();

                if (index + count < items.Count)
                {
                    // Hide visible rows.
                    foreach (var i in Enumerable.Range(index, count))
                    {
                        grid.RowDefinitions[1 + i].MaxHeight = 0.0;
                    }

                    // Update UI properties, including scrollViewer.ViewportHeight.
                    presenter.UpdateLayout();
                }

                index += count;
            }
        }
    }
}
