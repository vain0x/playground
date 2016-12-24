using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.WpfReportPrinting.Demo.SampleReports
{
    public sealed class OrderItem
    {
        public string Name { get; }
        public int Count { get; }
        public int UnitPrice { get; }
        public int TotalPrice { get; }
    }

    public sealed class OrderFormHeader
    {
        public string TargetName { get; }
        public DateTime DueDate { get; }
        public DateTime OrderDate { get; }
        public string OrdererName { get; }

        public OrderFormHeader(string targetName, DateTime dueDate, DateTime orderDate, string ordererName)
        {
            TargetName = targetName;
            DueDate = dueDate;
            OrderDate = orderDate;
            OrdererName = ordererName;
        }
    }

    public sealed class OrderFormPage
    {
        public OrderFormHeader Header { get; }
        public IReadOnlyList<OrderItem> Items { get; }

        public int PageIndex { get; }
        public int PageCount { get; set; } = -1;

        public OrderFormPage(OrderFormHeader header, int pageIndex, IReadOnlyList<OrderItem> items)
        {
            Header = header;
            PageIndex = pageIndex;
            Items = items;
        }
    }

    public sealed class OrderForm
        : IReport
    {
        public IReadOnlyList<object> DataContextList { get; }

        public OrderForm()
        {

        }
    }
}
