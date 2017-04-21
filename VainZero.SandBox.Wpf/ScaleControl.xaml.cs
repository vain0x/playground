using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Navigation;
using System.Windows.Shapes;

namespace VainZero.SandBox.Wpf
{
    /// <summary>
    /// ScaleControl.xaml の相互作用ロジック
    /// </summary>
    public partial class ScaleControl : UserControl
    {
        double scale;

        protected override Size MeasureOverride(Size availableSize)
        {
            scale = 1.0;
            var min = 0.01;

            while (scale > min)
            {
                var height = 0.0;
                var childSize = new Size(availableSize.Width / scale, double.PositiveInfinity);
                for (var i = 0; i < VisualChildrenCount; i++)
                {
                    var child = (UIElement)VisualTreeHelper.GetChild(this, i);
                    child.Measure(childSize);
                    height += child.DesiredSize.Height;
                }

                var availableHeight = availableSize.Height / scale;
                if (height <= availableHeight) break;
                scale = Math.Max(min, Math.Min(scale * 0.95, (scale * 2 + height / availableHeight) / 3));
            }

            return availableSize;
        }

        protected override Size ArrangeOverride(Size finalSize)
        {
            var scaleTransform = new ScaleTransform(scale, scale);
            var height = 0.0;
            for (var i = 0; i < VisualChildrenCount; i++)
            {
                var child = (UIElement)VisualTreeHelper.GetChild(this, i);
                child.RenderTransform = scaleTransform;
                child.Arrange(new Rect(new Point(0, scale * height), new Size(finalSize.Width / scale, child.DesiredSize.Height)));
                height += child.DesiredSize.Height;
            }

            return finalSize;
        }

        public ScaleControl()
        {
            InitializeComponent();
        }
    }
}
