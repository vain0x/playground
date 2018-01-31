using System;
using System.Collections.Generic;
using System.Diagnostics;
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
    /// DiagControl.xaml の相互作用ロジック
    /// </summary>
    public partial class DiagControl : UserControl
    {
        protected override Size MeasureOverride(Size constraint)
        {
            var stopwatch = Stopwatch.StartNew();
            try
            {
                return base.MeasureOverride(constraint);
            }
            finally
            {
                Console.WriteLine("Measure: {0}ms", stopwatch.ElapsedMilliseconds);
            }
        }

        protected override Size ArrangeOverride(Size arrangeBounds)
        {
            var stopwatch = Stopwatch.StartNew();
            try
            {
                return base.ArrangeOverride(arrangeBounds);
            }
            finally
            {
                Console.WriteLine("Arrange: {0}ms", stopwatch.ElapsedMilliseconds);
            }
        }

        protected override void OnRender(DrawingContext drawingContext)
        {
            var stopwatch = Stopwatch.StartNew();
            try
            {
                base.OnRender(drawingContext);
            }
            finally
            {
                Console.WriteLine("Render: {0}ms", stopwatch.ElapsedMilliseconds);
            }
        }

        public DiagControl()
        {
            InitializeComponent();
        }
    }
}
