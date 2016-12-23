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

        public IReadOnlyReactiveProperty<object> SelectedDataContext { get; }

        readonly ReactiveCommand printCommand =
            new ReactiveCommand();

        public ICommand PrintCommand => printCommand;

        public event EventHandler Printed;

        public ScaleSelector ScaleSelector { get; } =
            new ScaleSelector();

        public PaperSizeSelector PaperSizeSelector { get; } =
            new PaperSizeSelector();

        public void Print()
        {
            var printer = new ConcreteXamlPrinter();
            printer.Print(Report.Value.DataContextList);

            // 印刷したことを通知する。
            Printed?.Invoke(this, EventArgs.Empty);
        }

        public Previewer(IReadOnlyReactiveProperty<IReport> report)
        {
            Report = report;

            // TODO: ページセレクターを実装する。
            SelectedDataContext =
                Report.Select(r => r.DataContextList[0])
                .ToReadOnlyReactiveProperty();

            // 印刷ボタンが押されたら印刷する。
            printCommand.Subscribe(_ => Print());
        }
    }
}
