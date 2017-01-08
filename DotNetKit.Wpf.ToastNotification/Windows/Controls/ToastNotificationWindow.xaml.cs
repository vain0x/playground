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
using System.Windows.Threading;

namespace DotNetKit.Windows.Controls
{
    /// <summary>
    /// ToastNotificationWindow.xaml の相互作用ロジック
    /// </summary>
    public partial class ToastNotificationWindow : Window
    {
        void AdjustCore()
        {
            var workingArea = SystemParameters.WorkArea;
            var workingAreaCorner = new Point(workingArea.Right, workingArea.Bottom);
            var presentationSource = PresentationSource.FromVisual(this);
            var transform = presentationSource.CompositionTarget.TransformFromDevice;
            var corner = transform.Transform(workingAreaCorner);

            Left = corner.X - ActualWidth;
            Top = 0;
            Height = corner.Y;
        }

        void Adjust()
        {
            Dispatcher.BeginInvoke(
                DispatcherPriority.ApplicationIdle,
                new Action(AdjustCore)
            );
        }

        void OnSizeChanged(object sender, SizeChangedEventArgs e)
        {
            Adjust();
        }

        public
            ToastNotificationWindow(
                ToastNotificationCollection notifications,
                Window owner
            )
        {
            InitializeComponent();

            DataContext = notifications;
            Owner = owner;
            Height = 0;

            SizeChanged += OnSizeChanged;
        }
    }
}
