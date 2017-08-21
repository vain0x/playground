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

namespace ThreadingComponent
{
    public partial class CircularProgressBar
    {
        #region Fill
        public static readonly DependencyProperty FillProperty =
            DependencyProperty.Register(
                "Fill",
                typeof(Brush),
                typeof(CircularProgressBar),
                new PropertyMetadata(Brushes.Black)
            );

        public Brush Fill
        {
            get { return (Brush)GetValue(FillProperty); }
            set { SetValue(FillProperty, value); }
        }
        #endregion

        const int CircleCount = 9;
        const double Step = Math.PI * 2 / 10;

        static readonly double CanvasSize = 100;

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
            rotateTransform.Angle = (rotateTransform.Angle + 36) % 360;
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

            base.MeasureOverride(new Size(CanvasSize, CanvasSize));
            return new Size(l, l);
        }
        #endregion

        public CircularProgressBar()
        {
            InitializeComponent();

            var fillBinding = new Binding("Fill") { Source = this };

            circles = new Ellipse[CircleCount];
            for (var i = 0; i < circles.Length; i++)
            {
                var c =
                    new Ellipse()
                    {
                        Width = 20,
                        Height = 20,
                    };
                circles[i] = c;

                var s = (i + 1) / (double)CircleCount;
                c.Opacity = Math.Pow(s, 1.618);

                c.SetValue(Canvas.LeftProperty, 40 + Math.Cos(i * Step) * 40.0);
                c.SetValue(Canvas.TopProperty, 40 + Math.Sin(i * Step) * 40.0);

                c.SetBinding(Shape.FillProperty, fillBinding);

                canvas.Children.Add(c);
            }

            canvas.Width = CanvasSize;
            canvas.Height = CanvasSize;

            timer = new DispatcherTimer(DispatcherPriority.Render, Dispatcher);
            timer.Interval = new TimeSpan(0, 0, 0, 0, 17 * 6);
        }
    }
}
