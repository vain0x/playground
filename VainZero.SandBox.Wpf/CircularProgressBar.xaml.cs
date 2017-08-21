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
using System.Diagnostics;

namespace DotNetKit.Windows.Controls
{
    public partial class SpinningWheelControl
    {
        const double Epsilon = 1e-8;

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

        #region DotCount
        int dotCountField = 10;

        public int DotCount
        {
            get
            {
                return dotCountField;
            }
            set
            {
                if (value < 1) throw new ArgumentOutOfRangeException("value");
                dotCountField = value;
            }
        }
        #endregion

        #region DotRadius
        double dotRadiusField = 9;

        public double DotRadius
        {
            get
            {
                return dotRadiusField;
            }
            set
            {
                if (value <= Epsilon) throw new ArgumentOutOfRangeException("value");
                dotRadiusField = value;
            }
        }
        #endregion

        #region WheelRadius
        double wheelRadiusField = 40;

        public double WheelRadius
        {
            get
            {
                return wheelRadiusField;
            }
            set
            {
                if (value <= Epsilon) throw new ArgumentOutOfRangeException("value");
                wheelRadiusField = 40;
            }
        }
        #endregion

        #region IntervalMilliseconds
        double intervalMillisecondsField = 17 * 8;

        public double IntervalMilliseconds
        {
            get
            {
                return intervalMillisecondsField;
            }
            set
            {
                if (value <= Epsilon) throw new ArgumentOutOfRangeException("value");
                intervalMillisecondsField = value;

                timer.Interval = TimeSpan.FromMilliseconds(intervalMillisecondsField);
            }
        }
        #endregion

        #region RotateIndex
        int rotateIndexField = 0;

        int RotateIndex
        {
            get
            {
                return rotateIndexField;
            }
            set
            {
                Debug.Assert(value >= 0);

                rotateIndexField = value % DotCount;
                rotateTransform.Angle = rotateIndexField * (360.0 / DotCount);
            }
        }
        #endregion

        double CanvasSize
        {
            get
            {
                return (WheelRadius + DotRadius) * 2;
            }
        }

        readonly DispatcherTimer timer;

        void OnTick(object sender, EventArgs e)
        {
            RotateIndex++;
        }

        void Start()
        {
            RotateIndex = DotCount - 1;
            timer.Tick += OnTick;
            timer.Start();
        }

        void Stop()
        {
            timer.Stop();
            timer.Tick -= OnTick;
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

        protected override Size MeasureOverride(Size constraint)
        {
            var l =
                double.IsInfinity(constraint.Width) || double.IsInfinity(constraint.Height)
                    ? CanvasSize
                    : Math.Min(constraint.Width, constraint.Height);
            var scale = l / CanvasSize;
            scaleTransform.ScaleX = scale;
            scaleTransform.ScaleY = scale;
            return base.MeasureOverride(constraint);
        }

        public override void OnApplyTemplate()
        {
            base.OnApplyTemplate();

            canvas.Children.Clear();

            var dotCount = DotCount;
            var fillBinding = new Binding("Fill") { Source = this };

            for (var i = 0; i < dotCount; i++)
            {
                var c =
                    new Ellipse()
                    {
                        Width = DotRadius * 2,
                        Height = DotRadius * 2,
                    };

                c.Opacity = Math.Pow((double)(i + 1) / dotCount, 1.618);

                var t = 2 * Math.PI * i / dotCount;
                c.SetValue(Canvas.LeftProperty, (1 + Math.Cos(t)) * WheelRadius);
                c.SetValue(Canvas.TopProperty, (1 + Math.Sin(t)) * WheelRadius);

                c.SetBinding(Shape.FillProperty, fillBinding);

                canvas.Children.Add(c);
            }

            canvas.Width = CanvasSize;
            canvas.Height = CanvasSize;
        }

        public SpinningWheelControl()
        {
            InitializeComponent();

            timer = new DispatcherTimer(DispatcherPriority.Render, Dispatcher);
            timer.Interval = TimeSpan.FromMilliseconds(IntervalMilliseconds);
        }
    }
}
