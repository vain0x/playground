using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.WpfReportPrinting.Demo.SampleReports
{
    public interface IReport
    {
        IReadOnlyList<object> DataContextList { get; }
    }
}
