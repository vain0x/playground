using System.Collections.Generic;
using System.Linq;
using System.Reactive.Linq;
using Reactive.Bindings;
using VainZero.Windows.Documents;

namespace VainZero.WpfReportPrinting.Demo.Reports
{
    public sealed class ReportSelector
    {
        public IReadOnlyList<IReport> Reports { get; } =
            new IReport[]
            {
                new HelloWorldReport(),
                new OrderForm(),
            };

        public ReactiveProperty<IReport> SelectedReport { get; }

        public ReportSelector()
        {
            SelectedReport =
                new ReactiveProperty<IReport>(Reports.Last());
        }
    }
}
