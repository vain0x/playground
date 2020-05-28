using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;

namespace wpf_custom_panel
{
    /// <summary>
    /// 要素をセンタリングして、その左右に何かを配置するパネルを表す。
    /// 
    /// 子要素には、最大1つを除いて CenteringPanel.Dock に Left または Right を指定する。
    /// Left が指定された要素は左側に配置され、Right が指定された要素は右側に配置される。
    /// Dock が指定されていない要素があれば、それは中央に配置される。
    /// 左側に配置される要素と右側に配置される要素
    /// 
    /// 例: |-left-left- -center- -----right-|
    /// </summary>
    public sealed class CenteringPanel
        : Panel
    {
        public static readonly DependencyProperty DockProperty = DependencyProperty.RegisterAttached(
            "Dock",
            typeof(Dock?),
            typeof(CenteringPanel),
            new FrameworkPropertyMetadata()
            {
                AffectsArrange = true,
                DefaultValue = null,
            });

        public static Dock? GetDock(DependencyObject obj)
        {
            return (Dock?)obj.GetValue(DockProperty);
        }

        public static void SetDock(DependencyObject obj, Dock? value)
        {
            obj.SetValue(DockProperty, value);
        }

        static bool IsDockedOn(DependencyObject obj, Dock dock)
        {
            return GetDock(obj) == dock;
        }

        static Size AddSizeHorizontally(Size first, Size second)
        {
            return new Size(
                first.Width + second.Width,
                Math.Max(first.Height, second.Height)
            );
        }

        static Size SubSizeHorizontally(Size first, Size second)
        {
            return new Size(
                Math.Max(0, first.Width - second.Width),
                first.Height
            );
        }

        static Size MaxSize(Size first, Size second)
        {
            return new Size(
                Math.Max(first.Width, second.Width),
                Math.Max(first.Height, second.Height)
            );
        }

        static Size MinSize(Size first, Size second)
        {
            return new Size(
                Math.Min(first.Width, second.Width),
                Math.Min(first.Height, second.Height)
            );
        }

        protected override Size MeasureOverride(Size availableSize)
        {
            var centerSize = new Size();
            foreach (UIElement child in InternalChildren)
            {
                if (child.GetValue(DockProperty) != null) continue;

                child.Measure(availableSize);
                centerSize = child.DesiredSize;
                break;
            }

            var docks = new[]
            {
                Dock.Left,
                Dock.Right,
            };

            var maxSideSize = new Size();
            foreach (var dock in docks)
            {
                var sideSize = new Size();
                foreach (UIElement child in InternalChildren)
                {
                    if (!IsDockedOn(child, dock)) continue;

                    child.Measure(availableSize);
                    sideSize = AddSizeHorizontally(sideSize, child.DesiredSize);
                }
                maxSideSize = MaxSize(maxSideSize, sideSize);
            }

            var totalSideSize = new Size(maxSideSize.Width * 2, maxSideSize.Height);
            return AddSizeHorizontally(centerSize, totalSideSize);
        }

        protected override Size ArrangeOverride(Size finalSize)
        {
            var docks = new[]
            {
                Dock.Left,
                Dock.Right,
            };

            var centerSize = new Size();
            var sideSize = new Size(finalSize.Width / 2, finalSize.Height);

            foreach (UIElement child in InternalChildren)
            {
                if (child.GetValue(DockProperty) != null) continue;

                centerSize = MinSize(finalSize, child.DesiredSize);
                sideSize = new Size((finalSize.Width - centerSize.Width) / 2, finalSize.Height);
                var centerPoint = new Point(sideSize.Width, 0);

                child.Arrange(new Rect(centerPoint.X, centerPoint.Y, centerSize.Width, centerSize.Height));
                break;
            }

            foreach (var dock in docks)
            {
                var point = new Point(0, 0);
                var totalSize = sideSize;

                foreach (UIElement child in InternalChildren)
                {
                    if (!IsDockedOn(child, Dock.Left)) continue;

                    var size = new Size(Math.Min(totalSize.Width, child.DesiredSize.Width), totalSize.Height);

                    child.Arrange(new Rect(point.X, point.Y, size.Width, size.Height));

                    totalSize = SubSizeHorizontally(totalSize, size);
                    point.X += size.Width;
                }
            }

            foreach (var dock in docks)
            {
                var point = new Point(finalSize.Width, 0);
                var totalSize = sideSize;

                foreach (var child in InternalChildren.OfType<UIElement>().Where(child => IsDockedOn(child, Dock.Right)).Reverse())
                {
                    var size = new Size(Math.Min(totalSize.Width, child.DesiredSize.Width), totalSize.Height);
                    totalSize = SubSizeHorizontally(totalSize, size);
                    point.X -= size.Width;

                    child.Arrange(new Rect(point.X, point.Y, size.Width, size.Height));
                }
            }

            return finalSize;
        }
    }
}
