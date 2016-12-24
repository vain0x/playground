using System;
using System.Reactive.Linq;
using System.Windows.Input;
using Reactive.Bindings;
using VainZero.Windows.Printing;
using VainZero.WpfReportPrinting.Demo.SampleReports;

namespace VainZero.WpfReportPrinting.Demo.Previewing
{
    public sealed class Previewer
    {
        public IReadOnlyReactiveProperty<IReport> Report { get; }

        readonly ReactiveCommand printCommand =
            new ReactiveCommand();

        public ICommand PrintCommand => printCommand;

        public ScaleSelector ScaleSelector { get; } =
            new ScaleSelector();

        public PaperSizeSelector PaperSizeSelector { get; } =
            new PaperSizeSelector();

        public void Print()
        {
            var printer = new XamlPrinter();
            printer.PrintPages(Report.Value.DataContextList);
        }

        public Previewer(IReadOnlyReactiveProperty<IReport> report)
        {
            Report = report;

            // 印刷ボタンが押されたら印刷する。
            printCommand.Subscribe(_ => Print());
        }
    }
}
