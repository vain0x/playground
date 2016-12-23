using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.WpfReportPrinting.Demo.SampleReports
{
    public sealed class EmptyReport
        : IReport
    {
        public IReadOnlyList<object> DataContextList { get; } =
            new object[]
            {
                "Empty."
            };

        public static EmptyReport Instance { get; } =
            new EmptyReport();
    }
}
