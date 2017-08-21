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
        int dotCountCore = 10;

        public int DotCount
        {
            get
            {
                return dotCountCore;
            }
            set
            {
                if (value < 1) throw new ArgumentOutOfRangeException("value");
                dotCountCore = value;
            }
        }
        #endregion

        #region DotRadius
        double dotRadiusCore = 9;

        public double DotRadius
        {
            get
            {
                return dotRadiusCore;
            }
            set
            {
                if (value <= Epsilon) throw new ArgumentOutOfRangeException("value");
                dotRadiusCore = value;
            }
        }
        #endregion

        #region WheelRadius
        double wheelRadiusCore = 40;

        public double WheelRadius
        {
            get
            {
                return wheelRadiusCore;
            }
            set
            {
                if (value <= Epsilon) throw new ArgumentOutOfRangeException("value");
                wheelRadiusCore = 40;
            }
        }
        #endregion

        #region IntervalMilliseconds
        double intervalMillisecondsCore = 17 * 8;

        public double IntervalMilliseconds
        {
            get
            {
                return intervalMillisecondsCore;
            }
            set
            {
                if (value <= Epsilon) throw new ArgumentOutOfRangeException("value");
                intervalMillisecondsCore = value;

                timer.Interval = TimeSpan.FromMilliseconds(intervalMillisecondsCore);
            }
        }
        #endregion

        #region RotateIndex
        int rotateIndexCore = 0;

        int RotateIndex
        {
            get
            {
                return rotateIndexCore;
            }
            set
            {
                Debug.Assert(value >= 0);

                rotateIndexCore = value % DotCount;
                rotateTransform.Angle = rotateIndexCore * (360.0 / DotCount);
            }
        }
        #endregion

        double Step
        {
            get
            {
                return Math.PI * 2 / DotCount;
            }
        }

        double CenterDistance
        {
            get
            {
                return WheelRadius + DotRadius;
            }
        }

        double CanvasSize
        {
            get
            {
                return CenterDistance * 2;
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

                c.SetValue(
                    Canvas.LeftProperty,
                    CenterDistance + Math.Cos(i * Step) * WheelRadius - DotRadius
                );
                c.SetValue(
                    Canvas.TopProperty,
                    CenterDistance + Math.Sin(i * Step) * WheelRadius - DotRadius
                );

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
