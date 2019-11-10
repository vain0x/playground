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
        : ISingleHeaderedGridPage<OrderItem>
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

        public IReadOnlyList<object> Paginate(Size size)
        {
            var pages =
                new OrderFormPage(Header, Items)
                .Paginate(size)
                .Select(items => new OrderFormPage(Header, items))
                .ToArray();
            
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
