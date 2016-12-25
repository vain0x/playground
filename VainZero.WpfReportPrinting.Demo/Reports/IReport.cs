using VainZero.Windows.Documents;

namespace VainZero.WpfReportPrinting.Demo.Reports
{
    public interface IReport
        : IPaginatable
    {
        string ReportName { get; }
    }
}
