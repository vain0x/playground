using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
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
using ZXing;
using ZXing.Common;

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

            Task.Run(() =>
            {
                //PrintBarcode();
            });

            //Loaded += (sender, e) => CaptureAsync();
        }

        public ReactiveCollection<string> Messages { get; } = new ReactiveCollection<string>();

        void Log(string message)
        {
            Messages.Add(message);
        }

        public ReactiveProperty<string> ReadText { get; } = new ReactiveProperty<string>();

        async Task CaptureAsync()
        {
            try
            {
                var capture = new VideoCapture(0);
                //using (var win = new OpenCvSharp.Window("capture"))
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

                        //win.ShowImage(mat);

                        var stream = mat.ToMemoryStream(ext: ".bmp");
                        {
                            var bitmapImage = new BitmapImage();
                            bitmapImage.BeginInit();
                            bitmapImage.StreamSource = stream;
                            bitmapImage.EndInit();
                            bitmapImage.Freeze();
                            image.Source = bitmapImage;
                        }

                        var data = default(string);
                        ReadText.Value = TryRecognize(mat, out data) ? data : "?";

                        const double fps = 60;
                        await Task.Delay(TimeSpan.FromSeconds(1 / fps));
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


        #region バーコード認識
        bool TryRecognize(Mat imageMat, out string value)
        {
            using (var matG = imageMat.CvtColor(ColorConversionCodes.BGR2GRAY))
            using (var matB = matG.Threshold(0, 255, ThresholdTypes.Otsu))
            using (var image = matB.ToBitmap())
            {
                var reader = new BarcodeReader();
                reader.Options = new DecodingOptions()
                {
                    TryHarder = true,
                    PossibleFormats = new[] { BarcodeFormat.EAN_13 },
                };

                var results = reader.DecodeMultiple(image);
                if (results == null || results.Length == 0)
                {
                    value = default(string);
                    return false;
                }

                value = results.Select(x => x.Text).FirstOrDefault();
                return value != null;
            }
        }
        #endregion



        #region バーコード印刷
        private const BarcodeFormat DEFAULT_BARCODE_FORMAT = BarcodeFormat.EAN_13;
        private static readonly ImageFormat DEFAULT_IMAGE_FORMAT = ImageFormat.Bmp;
        private const String DEFAULT_OUTPUT_FILE = "out";
        private const int DEFAULT_WIDTH = 300;
        private const int DEFAULT_HEIGHT = 80;

        void PrintBarcode()
        {
            var barcodeFormat = DEFAULT_BARCODE_FORMAT;
            var imageFormat = DEFAULT_IMAGE_FORMAT;
            var outFileString = DEFAULT_OUTPUT_FILE;
            var width = DEFAULT_WIDTH;
            var height = DEFAULT_HEIGHT;
            var clipboard = false;

            var barcodeWriter = new BarcodeWriter
            {
                Format = barcodeFormat,
                Options = new EncodingOptions
                {
                    Height = height,
                    Width = width
                }
            };

            var bitmap = barcodeWriter.Write("000123456789");
            bitmap.Save("out.bmp", imageFormat);
        }

        private void Button_Click_1(object sender, RoutedEventArgs e)
        {
            PrintBarcode();
        }
        #endregion
    }
}
