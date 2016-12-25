using System;
using System.Collections.Generic;
using System.Reactive.Linq;
using System.Windows.Input;
using Reactive.Bindings;
using VainZero.Windows.Documents;
using VainZero.WpfReportPrinting.Demo.Printing;
using VainZero.WpfReportPrinting.Demo.Reports;

namespace VainZero.WpfReportPrinting.Demo.Previewing
{
    public sealed class Previewer
    {
        public IReadOnlyReactiveProperty<IReport> Report { get; }

        public IReadOnlyReactiveProperty<IReadOnlyList<object>> Pages { get; }

        readonly ReactiveCommand printCommand =
            new ReactiveCommand();

        public ICommand PrintCommand => printCommand;

        public ScaleSelector ScaleSelector { get; } =
            new ScaleSelector();

        public MediaSizeSelector MediaSizeSelector { get; } =
            new MediaSizeSelector();

        public void Print()
        {
            var report = Report.Value;
            var pageMediaSize = MediaSizeSelector.SelectedItem.Value.PageMediaSize;

            var printer = new Printer();
            printer.Print(report, pageMediaSize);
        }

        public Previewer(IReadOnlyReactiveProperty<IReport> report)
        {
            Report = report;

            Pages =
                Report.CombineLatest(
                    MediaSizeSelector.SelectedSize,
                    (r, pageSize) => r.Paginate(pageSize)
                )
                .ToReadOnlyReactiveProperty();

            // 印刷ボタンが押されたら印刷する。
            printCommand.Subscribe(_ => Print());
        }
    }
}
