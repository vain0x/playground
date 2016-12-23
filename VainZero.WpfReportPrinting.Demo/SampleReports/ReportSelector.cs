using System.Collections.Generic;
using System.Reactive.Linq;
using Reactive.Bindings;

namespace VainZero.WpfReportPrinting.Demo.SampleReports
{
    public sealed class ReportSelector
    {
        static KeyValuePair<string, IReport> ReportPair(string name, IReport report)
        {
            return new KeyValuePair<string, IReport>(name, report);
        }

        public IReadOnlyList<KeyValuePair<string, IReport>> Reports { get; } =
            new[]
            {
                ReportPair("ハローワールド", new HelloWorldReport()),
            };

        public IReadOnlyReactiveProperty<KeyValuePair<string, IReport>> SelectedItem { get; }

        public IReadOnlyReactiveProperty<IReport> SelectedReport { get; }

        public ReportSelector()
        {
            SelectedItem =
                new ReactiveProperty<KeyValuePair<string, IReport>>(Reports[0]);

            SelectedReport =
                SelectedItem.Select(item => item.Value).ToReadOnlyReactiveProperty();
        }
    }
}
