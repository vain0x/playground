using VainZero.WpfReportPrinting.Demo.Previewing;
using VainZero.WpfReportPrinting.Demo.SampleReports;

namespace VainZero.WpfReportPrinting.Demo
{
    public sealed class SampleReportPreviewer
    {
        public ReportSelector ReportSelector { get; } =
            new ReportSelector();

        public Previewer Previewer { get; }

        public SampleReportPreviewer()
        {
            Previewer = new Previewer(ReportSelector.SelectedReport);
        }
    }
}
