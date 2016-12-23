using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.WpfReportPrinting.Demo.SampleReports
{
    public sealed class HelloWorldPageViewModel
    {
    }

    public sealed class HelloWorldReport
        : IReport
    {
        public IReadOnlyList<object> DataContextList { get; } =
            new object[]
            {
                new HelloWorldPageViewModel(),
            };
    }
}
