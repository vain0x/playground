using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using VainZero.Windows.Documents;

namespace VainZero.WpfReportPrinting.Demo.Reports
{
    public sealed class HelloWorldPageViewModel
    {
    }

    public sealed class HelloWorldReport
        : IReport
    {
        public string ReportName => "ハローワールド";

        IReadOnlyList<object> Pages { get; } =
            new object[]
            {
                new HelloWorldPageViewModel(),
            };

        public IReadOnlyList<object> Paginate(Size size)
        {
            return Pages;
        }
    }
}
