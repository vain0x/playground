using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Threading;
using System.Windows.Input;
using System.Windows.Shapes;
using System.Windows.Data;
using System.Windows.Media;

namespace DotNetKit.Windows.Controls
{
    public partial class SpinningWheelControl
    {
        #region Fill
        public static readonly DependencyProperty FillProperty =
            DependencyProperty.Register(
                "Fill",
                typeof(Brush),
                typeof(SpinningWheelControl),
                new PropertyMetadata(Brushes.Black)
            );

        public Brush Fill
        {
            get { return (Brush)GetValue(FillProperty); }
            set { SetValue(FillProperty, value); }
        }
        #endregion

        const int CircleCount = 10;
        const double CircleRadius = 9;
        const double WheelRadius = 40;
        const double Step = Math.PI * 2 / CircleCount;

        const double CenterDistance = WheelRadius + CircleRadius;
        const double CanvasSize = CenterDistance * 2;

        readonly DispatcherTimer timer;
        readonly Ellipse[] circles;

        void Start()
        {
            timer.Tick += OnTick;
            timer.Start();
            rotateTransform.Angle = 0;
        }

        void Stop()
        {
            timer.Stop();
            timer.Tick -= OnTick;
        }

        void OnTick(object sender, EventArgs e)
        {
            rotateTransform.Angle = (rotateTransform.Angle + 360.0 / CircleCount) % 360;
        }

        void OnUnloaded(object sender, RoutedEventArgs e)
        {
            Stop();
        }

        void OnIsVisibleChanged(object sender, DependencyPropertyChangedEventArgs e)
        {
            var isVisible = (bool)e.NewValue;

            if (isVisible)
            {
                Start();
            }
            else
            {
                Stop();
            }
        }

        #region MeasureOverride
        double Length(Size size)
        {
            if (double.IsInfinity(size.Width) || double.IsInfinity(size.Height))
            {
                return CanvasSize;
            }

            return Math.Min(size.Width, size.Height);
        }

        protected override Size MeasureOverride(Size constraint)
        {
            var l = Length(constraint);
            var scale = l / CanvasSize;
            scaleTransform.ScaleX = scale;
            scaleTransform.ScaleY = scale;
            return base.MeasureOverride(constraint);
        }
        #endregion

        public SpinningWheelControl()
        {
            InitializeComponent();

            var fillBinding = new Binding("Fill") { Source = this };

            circles = new Ellipse[CircleCount];
            for (var i = 0; i < circles.Length; i++)
            {
                var c =
                    new Ellipse()
                    {
                        Width = CircleRadius * 2,
                        Height = CircleRadius * 2,
                    };

                c.Opacity = Math.Pow((double)(i + 1) / CircleCount, 1.618);

                c.SetValue(
                    Canvas.LeftProperty,
                    CenterDistance + Math.Cos(i * Step) * WheelRadius - CircleRadius
                );
                c.SetValue(
                    Canvas.TopProperty,
                    CenterDistance + Math.Sin(i * Step) * WheelRadius - CircleRadius
                );

                c.SetBinding(Shape.FillProperty, fillBinding);

                canvas.Children.Add(c);
                circles[i] = c;
            }

            canvas.Width = CanvasSize;
            canvas.Height = CanvasSize;

            timer = new DispatcherTimer(DispatcherPriority.Render, Dispatcher);
            timer.Interval = new TimeSpan(0, 0, 0, 0, 17 * 8);
        }
    }
}
