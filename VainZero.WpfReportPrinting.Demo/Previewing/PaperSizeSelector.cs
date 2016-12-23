using System;
using System.Collections.Generic;
using System.Linq;
using System.Reactive.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using Reactive.Bindings;

namespace VainZero.WpfReportPrinting.Demo.Previewing
{
    public sealed class PaperSize
    {
        public string Name { get; }
        public Size Size { get; }

        public PaperSize(string name, Size size)
        {
            Name = name;
            Size = size;
        }
    }

    public sealed class PaperSizeSelector
    {
        public static IReadOnlyList<PaperSize> Sizes { get; } =
            new[]
            {
                new PaperSize("A5", new Size(559, 794)),
                new PaperSize("A4", new Size(794, 1123)),
                new PaperSize("B5", new Size(688, 971)),
            };

        public ReactiveProperty<PaperSize> SelectedPaperSize { get; } =
            new ReactiveProperty<PaperSize>(Sizes[0]);

        public IReadOnlyReactiveProperty<Size> SelectedSize { get; }

        public PaperSizeSelector()
        {
            SelectedSize =
                SelectedPaperSize.Select(paperSize => paperSize.Size)
                .ToReadOnlyReactiveProperty();
        }
    }
}
