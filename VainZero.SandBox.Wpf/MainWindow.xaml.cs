using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reactive.Concurrency;
using System.Reactive.Disposables;
using System.Reactive.Linq;
using System.Reactive.Subjects;
using System.Reactive.Threading.Tasks;
using System.Text;
using System.Threading;
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
using OpenCvSharp;
using OpenCvSharp.Extensions;
using Reactive.Bindings;

namespace VainZero.SandBox.Wpf
{
    /// <summary>
    /// MainWindow.xaml の相互作用ロジック
    /// </summary>
    public partial class MainWindow : System.Windows.Window
    {
        public MainWindow()
        {
            InitializeComponent();

            DataContext = this;

            Loaded += (sender, e) => CaptureAsync();
        }

        public ReactiveCollection<string> Messages { get; } = new ReactiveCollection<string>();

        void Log(string message)
        {
            Messages.Add(message);
        }

        async Task CaptureAsync()
        {
            try
            {
                var capture = new VideoCapture(0);
                using (var win = new OpenCvSharp.Window("capture"))
                using (var mat = new Mat())
                {
                    while (true)
                    {
                        capture.Read(mat);
                        if (mat.Empty())
                        {
                            Log("empty");
                            break;
                        }

                        win.ShowImage(mat);
                        await Task.Delay(100);
                    }
                }
            }
            catch (Exception ex)
            {
                Messages.Add(ex.ToString());
            }
            Log("End Capture.");
        }

        private void Button_Click(object sender, RoutedEventArgs e)
        {
            CaptureAsync();
        }
    }
}
