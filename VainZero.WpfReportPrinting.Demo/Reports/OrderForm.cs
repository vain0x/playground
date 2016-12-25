using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using VainZero.Windows.Documents;

namespace VainZero.WpfReportPrinting.Demo.Reports
{
    public sealed class OrderItem
    {
        public string Name { get; }
        public int Count { get; }
        public int UnitPrice { get; }
        public int TotalPrice { get; }

        public OrderItem(string name, int unitPrice)
        {
            Name = name;
            Count = 1;
            UnitPrice = unitPrice;
            TotalPrice = unitPrice;
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

    public sealed class OrderItemList
    {
        public ObservableCollection<OrderItem> Items { get; }

        public OrderItemList(IEnumerable<OrderItem> items)
        {
            Items = new ObservableCollection<OrderItem>(items);
        }
    }

    public sealed class OrderFormPage
    {
        public OrderFormHeader Header { get; }
        public OrderItemList ItemList { get; }

        public int PageIndex { get; set; } = -1;
        public int PageCount { get; set; } = -1;

        public
            OrderFormPage(
                OrderFormHeader header,
                IEnumerable<OrderItem> items
            )
        {
            Header = header;
            ItemList = new OrderItemList(items);
        }
    }

    public sealed class OrderForm
        : IReport
    {
        public string ReportName => "注文書";

        public OrderFormHeader Header { get; }

        public OrderItemList ItemList { get; } =
            new OrderItemList(
                Enumerable.Range(1, 100)
                .Select(i => new OrderItem($"Item {i}", i * 100))
            );

        public IReadOnlyList<object> Paginate(Size size)
        {
            var pages = new List<OrderFormPage>();

            // TODO: Paginate.
            pages.Add(new OrderFormPage(Header, ItemList.Items));

            // 各ページのページ番号・ページ数を設定する。
            {
                var pageIndex = 1;
                foreach (var page in pages)
                {
                    page.PageIndex = pageIndex;
                    page.PageCount = pages.Count;
                    pageIndex++;
                }
            }

            return pages.ToArray();
        }

        public OrderForm()
        {
            Header =
                new OrderFormHeader(
                    "有限会社ターゲット",
                    new DateTime(2017, 01, 15),
                    ItemList.Items.Sum(item => item.TotalPrice)
                );
        }
    }
}
