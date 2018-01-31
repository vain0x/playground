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

            Loaded += (sender, e) => OnLoad();
        }

        void OnLoad()
        {
            Task.Run(() =>
            {
                //PrintBarcode();
            });

            //ShowBarcode();
            CaptureAsync();
        }

        void ShowBarcode()
        {
            using (var mat = LoadBarcode())
            {
                foreach (var p in new[] { 0, 0.25, 0.5 })
                {
                    using (var rotatedMat = Rotate(mat, p * 360))
                    {
                        Images.Add(BitmapImageFromMat(rotatedMat));
                    }
                }
            }
        }

        Mat LoadBarcode()
        {
            var data = File.ReadAllBytes("out.bmp");
            return Mat.FromStream(new MemoryStream(data), ImreadModes.Unchanged);
        }

        public ReactiveCollection<string> Messages { get; } = new ReactiveCollection<string>();

        void Log(string message)
        {
            Messages.Add(message);
        }

        public ReactiveProperty<string> ReadText { get; } = new ReactiveProperty<string>();

        public ReactiveCollection<BitmapImage> Images { get; } = new ReactiveCollection<BitmapImage>();

        static BitmapImage BitmapImageFromMat(Mat imageMat)
        {
            var stream = imageMat.ToMemoryStream(ext: ".bmp");
            var bitmapImage = new BitmapImage();
            bitmapImage.BeginInit();
            bitmapImage.StreamSource = stream;
            bitmapImage.EndInit();
            bitmapImage.Freeze();
            return bitmapImage;
        }

        Mat Rotate(Mat mat, double angle)
        {
            var size = Math.Max(mat.Width, mat.Height) * Math.Sqrt(2);
            var v = (int)Math.Round((size - mat.Height) / 2);
            var h = (int)Math.Round((size - mat.Width) / 2);
            var center = new Point2f((float)(size / 2), (float)(size / 2));
            return mat.CopyMakeBorder(v, v, h, h, BorderTypes.Constant).WarpAffine(Cv2.GetRotationMatrix2D(center, angle, 1), new OpenCvSharp.Size(size, size));
        }

        void OnRead(Mat mat)
        {
            previewImage.Source = BitmapImageFromMat(mat);

            const int iterationCount = 10;
            const int minLongSideWidth = 50;
            const int minShortSideWidth = 50;
            const int borderWidth = 10;

            Images.Clear();
            var list = new List<string>();

            using (var matG = mat.CvtColor(ColorConversionCodes.RGB2GRAY))
            using (var matB = matG.Threshold(0, 255, ThresholdTypes.Otsu))
            using (var matEroded = matB.Erode(null, iterations: iterationCount))
            {
                var conts = matEroded.FindContoursAsArray(RetrievalModes.List, ContourApproximationModes.ApproxSimple);
                foreach (var cont in conts)
                {
                    var rect = Cv2.BoundingRect(cont);

                    if (Math.Max(rect.Width, rect.Height) < minLongSideWidth ||
                        Math.Min(rect.Width, rect.Height) < minShortSideWidth)
                        continue;

                    var reader = new BarcodeReader();
                    reader.Options = new ZXing.Common.DecodingOptions
                    {
                        TryHarder = true,
                        PossibleFormats = new[] { BarcodeFormat.QR_CODE }.ToList()
                    };

                    using (var matR = new Mat(mat, rect))
                    using (var matRB = matR.CopyMakeBorder(borderWidth, borderWidth, borderWidth, borderWidth, BorderTypes.Constant, Scalar.White))
                    {
                        Images.Add(BitmapImageFromMat(matR));

                        using (var bitmap = matRB.ToBitmap())
                        {
                            var result = reader.Decode(bitmap);
                            if (result != null)
                            {
                                list.Add(result.Text);
                                break;
                            }
                        }
                    }
                }
            }

            ReadText.Value = string.Join(", ", list);
        }

        Mat Clamp(Mat mat, double maxWidth, double maxHeight)
        {
            var marginX = Math.Max(0, (int)Math.Ceiling((mat.Width - maxWidth) / 2));
            var marginY = Math.Max(0, (int)Math.Ceiling((mat.Height - maxHeight) / 2));
            var rect = new OpenCvSharp.Rect(marginX, marginX, mat.Width - marginX * 2, mat.Height - marginY * 2);
            return new Mat(mat, rect);
        }

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

                        var r = 200;
                        using (var matTrimmed = Clamp(mat, r, r))
                        {
                            OnRead(matTrimmed);
                        }

                        const double fps = 30;
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
        private const BarcodeFormat DEFAULT_BARCODE_FORMAT = BarcodeFormat.QR_CODE;
        private static readonly ImageFormat DEFAULT_IMAGE_FORMAT = ImageFormat.Bmp;
        private const String DEFAULT_OUTPUT_FILE = "out";
        private const int DEFAULT_WIDTH = 200;
        private const int DEFAULT_HEIGHT = 200;

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
