using System;
using System.Collections.Generic;
using System.Linq;
using System.Reactive.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using Reactive.Bindings;
using System.Windows.Media;
using System.Printing;

namespace VainZero.WpfReportPrinting.Demo.Previewing
{
    /// <summary>
    /// MediaSizeSelector の各要素を表す。
    /// </summary>
    public sealed class MediaSizeItem
    {
        public string Name { get; }
        public Size Size { get; }

        public MediaSizeItem(string name, Size size)
        {
            Name = name;
            Size = size;
        }
    }

    /// <summary>
    /// 印刷媒体の大きさを1つ選択するものを表す。
    /// </summary>
    public sealed class MediaSizeSelector
    {
        #region Items
        sealed class MediaSizeListBuilder
        {
            List<MediaSizeItem> List { get; } =
                new List<MediaSizeItem>();

            public MediaSizeListBuilder
                Add(
                    string name,
                    int widthMillimeter,
                    int heightMillimeter
                )
            {
                // NOTE: FixedPage は 96 dpi 固定。
                var dpi = 96.0;
                var dpmm = 0.03937 * dpi;
                var width = widthMillimeter * dpmm;
                var height = heightMillimeter * dpmm; 
                var size = new Size(width, height);
                List.Add(new MediaSizeItem(name, size));
                return this;
            }

            public MediaSizeItem[] ToArray()
            {
                return List.ToArray();
            }
        }

        public static IReadOnlyList<MediaSizeItem> Items { get; } =
            new MediaSizeListBuilder()
            .Add("A0", 841, 1189)
            .Add("A1", 594, 841)
            .Add("A2", 420, 594)
            .Add("A3", 297, 420)
            .Add("A4", 210, 297)
            .Add("A5", 148, 210)
            .Add("A6", 105, 148)
            .Add("A7", 74, 105)
            .Add("B3 (JIS)", 364, 515)
            .Add("B4 (JIS)", 257, 364)
            .Add("B5 (JIS)", 182, 257)
            .Add("B6 (JIS)", 128, 182)
            .ToArray();
        #endregion

        public ReactiveProperty<MediaSizeItem> SelectedItem { get; } =
            new ReactiveProperty<MediaSizeItem>(Items[5]); // A5

        public IReadOnlyReactiveProperty<Size> SelectedSize { get; }

        public MediaSizeSelector()
        {
            SelectedSize =
                SelectedItem.Select(item => item.Size)
                .ToReadOnlyReactiveProperty();
        }
    }
}
