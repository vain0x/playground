using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Reactive.Bindings;

namespace VainZero.WpfReportPrinting.Demo.Previewing
{
    /// <summary>
    /// プレビュー表示の縮尺を1つ選択するものを表す。
    /// </summary>
    public sealed class ScaleSelector
    {
        public static IReadOnlyList<double> Scales { get; } =
            new[]
            {
                0.50,
                0.667,
                0.75,
                1.0,
                1.25,
                1.333,
                1.5,
            };

        public ReactiveProperty<double> SelectedScale { get; } =
            new ReactiveProperty<double>(1.0);
    }
}
