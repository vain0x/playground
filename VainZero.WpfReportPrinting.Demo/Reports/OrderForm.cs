using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Media;
using VainZero.Windows.Controls;
using VainZero.Windows.Documents;
using VainZero.Windows.Media;

namespace VainZero.WpfReportPrinting.Demo.Reports
{
    public sealed class OrderItem
    {
        static Random Random { get; } =
            new Random();

        public string Name { get; }
        public int Count { get; }
        public int UnitPrice { get; }
        public int TotalPrice { get; }
        public string Note { get; }

        public OrderItem(string name, int unitPrice)
        {
            Name = name;
            Count = 1;
            UnitPrice = unitPrice;
            TotalPrice = unitPrice;

            // ページネーションの難度を上げるために、ランダムな行数の備考を生成する。
            Note =
                string.Join(
                    Environment.NewLine,
                    Enumerable.Range(0, Random.Next(0, 3)).Select(i => $"{i + 1}行目")
                );
        }
    }

    public sealed class OrderFormHeader
    {
        public string TargetName { get; }
        public DateTime OrderDate { get; }
        public int TotalPrice { get; }

        public
            OrderFormHeader(
                string targetName,
                DateTime orderDate,
                int totalPrice
            )
        {
            TargetName = targetName;
            OrderDate = orderDate;
            TotalPrice = totalPrice;
        }
    }

    public sealed class OrderFormPage
    {
        public OrderFormHeader Header { get; }
        public IReadOnlyList<OrderItem> Items { get; }

        public int PageIndex { get; set; } = -1;
        public int PageCount { get; set; } = -1;

        public
            OrderFormPage(
                OrderFormHeader header,
                IReadOnlyList<OrderItem> items
            )
        {
            Header = header;
            Items = items;
        }
    }

    public sealed class OrderForm
        : IReport
    {
        public string ReportName => "注文書";

        public OrderFormHeader Header { get; }

        public IReadOnlyList<OrderItem> Items { get; } =
            Enumerable.Range(1, 50)
            .Select(i => new OrderItem($"Item {i}", i * 100))
            .ToArray();

        public IEnumerable<OrderFormPage> PaginateCore(Size size)
        {
            // ページネーションを行うために、実際に DataGrid を生成する。
            var preview = new OrderFormPage(Header, Items);
            var presenter =
                new ContentPresenter()
                {
                    Content = preview,
                    Width = size.Width,
                    Height = size.Height,
                };

            presenter.Measure(size);
            presenter.Arrange(new Rect(new Point(0, 0), size));
            presenter.UpdateLayout();

            var headeredGrid =
                presenter.VisualDescendantsBreadthFirstOrder().OfType<HeaderedGrid>().First();

            var scrollViewer = headeredGrid.ScrollViewer;
            var grid = headeredGrid.Grid;

            var items = preview.Items;
            var index = 0;
            while (index < items.Count)
            {
                // 表示されている行数を計算する。
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
                    // ヘッダー行が大きすぎてコンテンツを表示できていない。
                    throw new Exception();
                }

                var pageItems =
                    Enumerable.Range(index, count).Select(i => items[i]).ToArray();

                // 1画面に表示できた行からなるページを追加する。
                yield return new OrderFormPage(Header, pageItems);

                if (index + count < items.Count)
                {
                    // いまのページに追加された分の行を非表示 (高さ 0) にする。
                    foreach (var i in Enumerable.Range(index, count))
                    {
                        grid.RowDefinitions[1 + i].MaxHeight = 0.0;
                    }

                    // scrollViewer.ViewportHeight を更新する。
                    presenter.UpdateLayout();
                }

                index += count;
            }
        }

        public IReadOnlyList<object> Paginate(Size size)
        {
            var pages = PaginateCore(size).ToArray();
            
            // 各ページのページ番号・ページ数を設定する。
            var pageIndex = 1;
            foreach (var page in pages)
            {
                page.PageIndex = pageIndex;
                page.PageCount = pages.Length;
                pageIndex++;
            }

            return pages;
        }

        public OrderForm()
        {
            Header =
                new OrderFormHeader(
                    "株式会社ほげほげ",
                    new DateTime(2017, 01, 15),
                    Items.Sum(item => item.TotalPrice)
                );
        }
    }
}
